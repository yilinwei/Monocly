package optics.core

trait CoindexedOptic[-C[_[_, _]], +E, -S, +T, +A, -B] { self =>

  def apply[F[-_, +_]](fab: F[(E, A), B])(using C[F] & Profunctor[F]): F[S, (E, T)]

  final inline def <<<[CC <: C, EE, U, V](
    other: CoindexedOptic[CC, EE, U, V, S, T]
  ): CoindexedOptic[CC, E | EE, U, V, A, B] =
    new CoindexedOptic {
      def apply[F[-_, +_]](fab: F[(E | EE, A), B])(using profunctor: CC[F] & Profunctor[F]) =
        other(profunctor.dimap(self(fab))(_._2, _._2))
    }

}


type CoindexedPrism[+E, -S, +T, +A, -B] = CoindexedOptic[Cocartesian, E, S, T, A, B]
type CoindexedAffineTraversal[+E, -S, +T, +A, -B] = CoindexedOptic[[F[_, _]] =>> Cocartesian[F] & Cartesian[F], E, S, T, A, B]

object CoindexedOptic {

  extension on [E, S, T, A, B](prism: CoindexedAffineTraversal[E, S, T, A, B]) {
    def preview: S => Either[(E, T), A] =
      prism(ForgetE(Right(_))).run.andThen(_.map(_._2))
  }

  extension on [E, S, T, A, B](lens: CoindexedLens[E, S, T, A, B]) {
    def view: S => A =
      lens(Forget(identity)).run.andThen(_._2)
  }

}

object CoindexedPrism {

  def apply[S, E, T, A, B](
    getter: S => Either[(E, T), (E, A)],
    setter: B => (E, T),
  ): CoindexedPrism[E, S, T, A, B] =
    new CoindexedOptic {
      def apply[F[-_, +_]](fab: F[(E, A), B])(using profunctor: Cocartesian[F] & Profunctor[F]): F[S, (E, T)] = {
        import profunctor._
        dimap(right(fab))(getter, _.fold(identity, setter))
      }
    }

}

type CoindexedPrismM[+E, S, A] = CoindexedPrism[E, S, S, A, A]

object CoindexedPrismM {
  def apply[E, S, A](getter: S => Either[(E, S), (E, A)], setter: A => (E, S)) = CoindexedPrism(getter, setter)
}

type CoindexedLens[+E, -S, +T, +A, -B] = CoindexedOptic[Cartesian, E, S, T, A, B]

object CoindexedLens {

  def apply[S, E, T, A, B](
    getter: S => (E, A),
    setter: S => B => (E, T)
  ): CoindexedLens[E, S, T, A, B] =
    new CoindexedOptic {
      def apply[F[-_, +_]](fab: F[(E, A), B])(using profunctor: Cartesian[F] & Profunctor[F]): F[S, (E, T)] = {
        import profunctor._
        dimap(first(fab))(s => (getter(s), s), (b, s) => setter(s)(b))
      }
    }
}

type CoindexedLensM[+E, S, A] = CoindexedLens[E, S, S, A, A]

object CoindexedLensM {

  def apply[E, S, A](
    getter: S => (E, A),
    setter: S => A => (E, S)
  ): CoindexedLensM[E, S, A] =
    CoindexedLens(getter, setter)

}

type LensM[S, A] = CoindexedLensM[Unit, S, A]

object LensM {

  def apply[S, A](
    getter: S => A,
    setter: S => A => S
  ): LensM[S, A] =
    CoindexedLens(getter.andThen(((), _)), s => a => ((), setter(s)(a)))

}


sealed trait PrismProduct

object PrismProduct {

  case class Foo(i: Int) extends PrismProduct
  case class Bar(s: String) extends PrismProduct
}

object Test {

  val prism = CoindexedPrismM[String | Unit, PrismProduct, PrismProduct.Foo](
    s => s match {
      case foo: PrismProduct.Foo => Right(((), foo))
      case bar: PrismProduct.Bar => Left(("Not a match!", bar))
    },
    foo => ((), foo)
  )

  val lens = LensM[PrismProduct.Foo, Int](_.i, s => a => s.copy(i = a))

  def main(args: Array[String]): Unit = {
    val f = (lens <<< prism).preview
    println(f(PrismProduct.Bar("asd")))
  }

}
