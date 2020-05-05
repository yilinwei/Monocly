package optics.core


trait Invariant[F[_]]

trait Functor[F[_]] extends Invariant[F] {
  def map[A, B](f: A => B)(fa: F[A]): F[B]
}

/** We force this at the top level*/
trait Invariant2[F[_, _]] {
}

trait Bifunctor[F[_, _]] extends Invariant2[F] {
  def bimap[A, B, C, D](fac: F[A, C])(f: A => B, g: C => D): F[B, D]
}

trait Bicontravariant[F[_, _]] extends Invariant2[F] {
  def cimap[A, B, C, D](fac: F[A, C])(f: B => A, G: D => C): F[B, D]
}

trait Profunctor[F[_, _]] extends Invariant2[F] {
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A, g: B => D): F[C, D]
}

trait Cartesian[F[_, _]] extends Profunctor[F] {
  def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
}

trait Cocartesian[F[_, _]] extends Profunctor[F] {
  def right[A, B, C](fa: F[A, B]): F[Either[C, A], Either[C, B]]
}

trait Wander[F[_, _]] extends Profunctor[F]

object Invariant2 {

  given as Cartesian[Function1] with Cocartesian[Function1] {

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
