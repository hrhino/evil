package test

import example._

@addFooMethod class A

@addFooMethod("bar") class B

@example.addFooMethod class C

@decoy.addFooMethod class D

import example.{addFooMethod => afm}

@afm class E

object Test extends App {
  List[Int](
    new A().foo,
    new B().bar,
    new C().foo,
    // new D().foo
    new E().foo,
  ) foreach (n => assert(n == 42))

  assert(!classOf[D].getMethods.map(_.getName).contains("foo"))

  println("all good")

}
