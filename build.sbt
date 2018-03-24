scalaVersion := "2.12.5"
libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
scalacOptions in Test ++= Seq(
  s"-Xplugin:${(packageBin in Compile).value}",
  s"-Jdummy=${System.currentTimeMillis()}",
)
