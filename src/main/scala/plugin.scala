package example

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.reflect.internal.Mode._

class ExamplePlugin(val global: Global) extends Plugin {
  val name = "example"
  val components = Nil
  val description = ""

  import global._

  val synthetics = perRunCaches.newAnyRefMap[Symbol, Tree]()

  lazy val AnnCls = symbolOf[addFooMethod]
  lazy val AnnNme = AnnCls.name
  case object NeedsFooMethodAdded

  object Impl extends analyzer.AnalyzerPlugin with analyzer.MacroPlugin {
    import analyzer.{global => _, _}

    override def isActive() = global.phase.id < global.currentRun.picklerPhase.id

    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {
      val ctx = typer.context
      defTree match {
        case classImpl: Template if ctx.owner.isClass =>
          val annPairs: List[(AnnotationInfo, Tree)] = ctx.owner.annotations zip {
            ctx.outer.tree match {
              case ClassDef(mods, _, _, _) =>
                mods.annotations
              case _ => Nil
            }
          }
          //inform(s"${ctx.owner}: $annPairs")
          val maybe = annPairs.collectFirst {
            case (ai, Apply(Select(New(tpt), nme.CONSTRUCTOR), args))
                if {
                  newTyper(ctx.outer)
                    .typed(tpt.duplicate, TYPEmode, WildcardType)
                    .symbol eq AnnCls
                } =>
              args.headOption
          }
          //inform(s"\t${ctx.owner}: $maybe")
          maybe.foreach { providedName =>
            val theName = providedName match {
              case None => TermName("foo")
              case Some(Literal(Constant(name: String))) => TermName(name)
              case Some(other) => globalError(other.pos, "literal string pls"); nme.ERROR
            }
            //val target = ctx.owner
            val defn = DefDef(Modifiers(), theName, Nil, Nil, TypeTree(), Literal(Constant(42)))
            typer.namer.enterDefDef(defn)
            synthetics(defn.symbol) = defn
            ctx.owner.updateAttachment(NeedsFooMethodAdded)
          }
        case _ =>
      }
      tpe
    }

    override def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
      if (typer.context.owner.hasAttachment[NeedsFooMethodAdded.type]) {
        assert(typer.context.owner.hasCompleteInfo)
        val toAdd = typer.context.owner.info.decls.toList.flatMap(synthetics.get)
        toAdd ::: stats
      } else stats
    }
  }

  analyzer.addAnalyzerPlugin(Impl)
  analyzer.addMacroPlugin(Impl)
}