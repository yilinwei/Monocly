package optics.poly

import functions.Index

class IndexTest extends munit.FunSuite {

  case class Foo(i: Int)
  case class Bar(l: List[Foo], m: Map[String, Foo])

  val lLens = Lens[Bar, List[Foo]](_.l, ll => _.copy(l = ll))
  val mLens = Lens[Bar, Map[String, Foo]](_.m, mm => _.copy(m = mm))
  val iLens = Lens[Foo, Int](_.i, ii => _.copy(i = ii))


  test("inference") {
    assertEquals(
      (lLens >>> Index(0) >>> iLens).getOrError(Bar(List(Foo(1)), Map())),
      Right(1)
    )
    assertEquals(
      (mLens >>> Index("moo") >>> iLens).getOrError(Bar(List(), Map("moo" -> Foo(1)))),
      Right(1)
    )
  }

}
