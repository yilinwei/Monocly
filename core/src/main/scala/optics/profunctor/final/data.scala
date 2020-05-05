package optics.core

opaque type Forget[R, -A, +B] = A => R

object Forget {

  def apply[R, A, B](f: A => R): Forget[R, A, B] = f

  extension on [R, A, B](forget: Forget[R, A, B]) {
    def run: A => R = {
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

opaque type ForgetE[R, -A, +B] = A => Either[B, R]

object ForgetE {

  def apply[R, A, B](f: A => Either[B, R]): ForgetE[R, A, B] = f

  def run[R, A, B](forget: ForgetE[R, A, B]): A => Either[B, R] = forget

  extension on [R, A, B](forget: ForgetE[R, A, B]) {
    def run: A => Either[B, R] = {
      forget
    }
  }

  given instance[R] as Cocartesian[[A, B] =>> ForgetE[R, A, B]] with Cartesian[[A, B] =>> ForgetE[R, A, B]]
  {

    def right[A, B, C](fab: ForgetE[R, A, B]): ForgetE[R, Either[C, A], Either[C, B]] = {
      Forget(_.map(fab.run) match {
        case Left(a) => Left(Left(a))
        case Right(Left(b)) => Left(Right(b))
        case Right(Right(c)) => Right(c)
      })
    }

    def dimap[A, B, C, D](fa: ForgetE[R, A, B])(f: C => A, g: B => D): ForgetE[R, C, D] = {
      Forget(fa.run.compose(f).andThen(_.left.map(g)))
    }

    def first[A, B, C](fa: ForgetE[R, A, B]): ForgetE[R, (A, C), (B, C)] =
      ForgetE((a, c) => fa.run(a).left.map((_, c)))

  }

}

opaque type Tagged[-A, +B] = B

object Tagged {

  def apply[A, B](b: B): Tagged[A, B] = b

  given as Profunctor[Tagged] with Bifunctor[Tagged] {
    def bimap[A, B, C, D](fac: Tagged[A, C])(f: A => B, g: C => D): Tagged[B, D] =
      Tagged(g(fac))
    def dimap[A, B, C, D](fab: Tagged[A, B])(f: C => A, g: B => D): Tagged[C, D] =
      Tagged(g(fab))
  }

}

opaque type Star[F[_], A, B] = A => F[B]

object Star {

  def apply[F[_], A, B](f: A => F[B]): Star[F, A, B] = f

  given [F[_]](using functor: Functor[F]) as Profunctor[[A, B] =>> Star[F, A, B]] {
    def dimap[A, B, C, D](fab: Star[F, A, B])(f: C => A, g: B => D): Star[F, C, D] =
      Star(fab.andThen(functor.map(g)).compose(f))

  }

}
