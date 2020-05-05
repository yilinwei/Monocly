package optics.profunctor.mono

import optics.profunctor.{poly => P}

type Optic[C[_[_, _]], A, B] = P.Optic[C, A, A, B, B]
type Lens[A, B] = Optic[P.Cartesian, A, B]
type Fold[S, A] = Optic[[F[_, _]] =>> P.Monoidal[F] & P.Bicontravariant[F], S, A]
type Getter[A, B] = Optic[[F[_, _]] =>> P.Bicontravariant[F], A, B]
type Review[T, B] = Optic[[F[_, _]] =>> P.Bifunctor[F] & P.Cartesian[F], T, B]

object Lens {

  def apply[A, B](getter: A => B, setter: A => B => A): Lens[A, B] =
    P.Lens(getter, setter)

}

case class Foo(bar: Int)

object Mono {

  def main(args: Array[String]): Unit = {
    // val lens = Lens[Foo, Int](_.bar, foo => bar => foo.copy(bar = bar))

    val lens: Lens[Foo, Int] = ???
    val getter: Getter[Int, String] = ???
    val lens2: Lens[String, Foo] = ???

    val a = getter.get(3)
    // val getter = Getter[Foo, Int](_.bar)
    // val moo = lens.get(Foo(32))
    // val doo = getter.get(Foo(32))

    // println(moo)
    // println(moo)
  }


}
