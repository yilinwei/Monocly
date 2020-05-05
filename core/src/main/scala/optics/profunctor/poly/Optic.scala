package optics.profunctor.poly

import scala.annotation.alpha

trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](fa: F[A, B])(f: A => B, g: C => D): F[B, D]
}

trait Bicontravariant[F[_, _]] {
  def cimap[A, B, C, D](fa: F[A, C])(f: B => A, G: D => C): F[B, D]
}

trait Profunctor[F[_, _]] {
  def dimap[A, B, C, D](fa: F[A, B])(f: C => A, g: B => D): F[C, D]
}

trait Closed[F[_, _]] extends Profunctor[F] {

}

trait Cartesian[F[_, _]] extends Profunctor[F] {
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
}

trait Cocartesian[F[_, _]] extends Profunctor[F] {
  def right[A, B, C](fa: F[A, B]): F[Either[C, A], Either[C, B]]
}

//TODO: ?
trait Monoidal[F[_, _]] extends Profunctor[F] {
  def par[A, B, C, D](fab: F[A, B], fcd: F[C, D]): F[(A, C), (B, D)]
}

trait Applicative[F[+_]]{
  def pure[A](value: A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Profunctor {

  given as Cartesian[Function1] with Cocartesian[Function1] with Monoidal[Function1] {

    def dimap[A, B, C, D](fab: A => B)(f: C => A, g: B => D): C => D =
      f.andThen(fab).andThen(g)

    def right[A, B, C](fab: A => B): Either[C, A] => Either[C, B] =
      _.map(fab)

    def par[A, B, C, D](fab: A => B, fcd: C => D): ((A, C)) => (B, D) =
      (a, b) => (fab(a), fcd(b))

    def first[A, B, C](fa: A => B):((A, C)) => (B, C) =
      (a, c) => (fa(a), c)
  }

}

opaque type Forget[R, -A, +B] = A => R

object Forget {

  def apply[R, A, B](f: A => R): Forget[R, A, B] = f
  def unwrap[R, A, B](forget: Forget[R, A, B]): A => R = forget


  extension on [R, A, B](forget: Forget[R, A, B]) {

    def apply(a: A): R = {
      forget.function(a)
    }

    def function: A => R = {
      val f: A => R = forget
      f
    }


  }

  given [R] as Cartesian[[A, B] =>> Forget[R, A, B]] with Bicontravariant[[A, B] =>> Forget[R, A, B]] {

    def dimap[A, B, C, D](fa: Forget[R, A, B])(f: C => A, g: B => D): Forget[R, C, D] =
      Forget(fa.compose(f))

    def cimap[A, B, C, D](fa: Forget[R, A, C])(f: B => A, G: D => C): Forget[R, B, D] =
      Forget(fa.compose(f))

    def first[A, B, C](fa: Forget[R, A, B]): Forget[R, (A, C), (B, C)] =
      Forget(fa.compose(_._1))

  }
}

sealed trait Optic[-C[_[_, _]], -S, +T, +A, -B] { self =>

  def apply[F[-_, +_]](pab: F[A, B])(using C[F]): F[S, T]

  @alpha("compose")
  final def <<<[C1 <: C, U, V](other: Optic[C1, U, V, S, T]): Optic[C1, U, V, A, B] = {
    new Optic {
      def apply[F[-_, +_]](pab: F[A, B])(using C1[F]): F[U, V] =
        other(self(pab))
    }
  }

  @alpha("andThen")
  final def >>>[C1 <: C, U, V](other: Optic[C1, A, B, U, V]) =
    other <<< self

}

type Lens[-S, +T, +A, -B] = Optic[Cartesian, S, T, A, B]
type Prism[-S, +T, +A, -B] = Optic[Cocartesian, S, T, A, B]
type Adapter[-S, +T, +A, -B] = Optic[Profunctor, S, T, A, B]
type Affine[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Cartesian[F] & Cocartesian[F], S, T, A, B]
type Traversal[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Monoidal[F] & Cartesian[F] & Cocartesian[F], S, T, A, B]
type Getter[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Bicontravariant[F], S, T, A, B]

object Optic {

  extension on [S, T, A, B](getter: Getter[S, T, A, B])  {
    /** `get` */
    inline def get: S => A =
      getter(Forget(identity)).function
  }

  extension on [S, T, A, B](lens: Lens[S, T, A, B])  {
    /** `get` */
    inline def get: S => A =
      lens(Forget(identity)).function
  }

  extension on [S, T, A, B](traversal: Traversal[S, T, A, B]) {
    /** `modify` */
    inline def modify(f: A => B): S => T =
      traversal(f)
  }

}

object Lens {

  def apply[S, T, A, B](
    getter: S => A,
    setter: S => B => T
  ): Lens[S, T, A, B] =
    new Optic {
      def apply[F[-_, +_]](pab: F[A, B])(using cartesian: Cartesian[F]): F[S, T] =
        cartesian.dimap(cartesian.first(pab))(s => (getter(s), s), (b, s) => setter(s)(b))
    }

}
