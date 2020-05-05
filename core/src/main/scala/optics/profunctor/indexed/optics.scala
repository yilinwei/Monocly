package profunctor.optics.core

import scala.compiletime._

// import scala.annotation.alpha

// /** A `Profunctor` `F[_, _]` which has the index `I` and co-index `E`*/
// trait IxCoixProfunctor[-I, +E, F[_, _, _, _]] {

//   //The composition of the carrier is important, if we put the index

//   def ilmap[A, B, C, D, J <: I, K, E1 >: E](fa: F[J, E1, A, B])(f: K => J): F[K, E1, A, B] = ???
//   def dimap[A, B, C, D, J <: I, E1 >: E](fa: F[J, E1, A, B])(f: C => A, g: B => D): F[J, E1, C, D]

// }


// type Profunctor[F[_, _, _, _]] = IxCoixProfunctor[Any, Nothing, F]

// /** A `Cartesian` `F[_, _]` which has the index `I` and co-index `E`*/
// trait IxCoixCartesian[-I, +E, F[_, _, _, _]] extends Profunctor[F] {
//   def first[A, B, C, J <: I, E1 >: E](fa: F[J, E1, A, B]): F[J, E1, (A, C), (B, C)] = ???
// }

// type Cartesian[F[_, _, _, _]] = IxCoixCartesian[Any, Nothing, F]

// /** A `Cocartesian` `F[_, _]` which has the index `I` and co-index `E`*/
// trait IxCoixCocartesian[-I, +E, F[_, _, _, _]] extends Profunctor[F] {
//   // def right[A, B, C, J <: I, E1 <: E](fa: F[A, B]): F[Either[C, A], Either[C, B]]
// }

// // // type Cocartesian[F[_, _]] = IxCoixCocartesian[Any, Nothing, F]

// /** A `Wandering` `F[_, _]` which has the index `I` and co-index `E`*/
// trait IxCoixWandering[-I, +E, F[_, _, _, _]] extends Profunctor[F] {
// }

// trait IxCoixBicontravariant[-I, +E, F[_, _, _, _]] {

// }
// // trait Monoidal[F[_, _]] extends Profunctor[F] {
// //   def par[A, B, C, D](fab: F[A, B], fcd: F[C, D]): F[(A, C), (B, D)]
// // }

// // trait Applicative[F[+_]]{
// //   def pure[A](value: A): F[A]
// //   def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
// //   def map[A, B](fa: F[A])(f: A => B): F[B]
// // }

// opaque type ForgetK[F[_, _], A, B, C, D] = F[C, D]

// object ForgetK {

//   def apply[F[_, _], A, B, C, D](f: F[C, D]): ForgetK[F, A, B, C, D] = f

// }

// object Profunctor {

//   given as Cartesian[[I, E, A, B] =>> ForgetK[Function1, I, E, A, B]] {
//     def dimap[A, B, C, D, J <: Any, E1 >: Nothing](fab: ForgetK[Function1, J, E1, A, B])(f: C => A, g: B => D): ForgetK[Function1, J, E1, C, D] = {
//       val h: A => B = fab
//       ForgetK(h.compose(f).andThen(g))
//     }
//   }

//   // given as [I, E] as IxCoixCartesian[[I, E, A, B] => ]

//   // given as Cartesian[Function1] with Cocartesian[Function1] with Monoidal[Function1] {

//   //   def dimap[A, B, C, D](fab: A => B)(f: C => A, g: B => D): C => D =
//   //     f.andThen(fab).andThen(g)

//   //   // def right[A, B, C](fab: A => B): Either[C, A] => Either[C, B] =
//   //   //   _.map(fab)

//   //   // def par[A, B, C, D](fab: A => B, fcd: C => D): ((A, C)) => (B, D) =
//   //   //   (a, b) => (fab(a), fcd(b))

//   //   // def first[A, B, C](fa: A => B):((A, C)) => (B, C) =
//   //   //   (a, c) => (fa(a), c)
//   // }


// }



// // opaque type Forget[R, -A, +B] = A => R

// // object Forget {

// //   def apply[R, A, B](f: A => R): Forget[R, A, B] = f
// //   def unwrap[R, A, B](forget: Forget[R, A, B]): A => R = forget


// //   extension on [R, A, B](forget: Forget[R, A, B]) {

// //     def apply(a: A): R = {
// //       forget.function(a)
// //     }

// //     def function: A => R = {
// //       val f: A => R = forget
// //       f
// //     }


// //   }

// //   given [R] as Cartesian[[A, B] =>> Forget[R, A, B]] with Bicontravariant[[A, B] =>> Forget[R, A, B]] {

// //     def dimap[A, B, C, D](fa: Forget[R, A, B])(f: C => A, g: B => D): Forget[R, C, D] =
// //       Forget(fa.compose(f))

// //     def cimap[A, B, C, D](fa: Forget[R, A, C])(f: B => A, G: D => C): Forget[R, B, D] =
// //       Forget(fa.compose(f))

// //     def first[A, B, C](fa: Forget[R, A, B]): Forget[R, (A, C), (B, C)] =
// //       Forget(fa.compose(_._1))

// //   }
// // }


// /** A profunctor `Optic` which is indexed on `I` and coindexed on `E`. */
// sealed trait Optic[-C[-_, +_, _[_, _, _, _]], -I, +E, -S, +T, +A, -B] { self =>

//   /** Applies the `Optic` to an arbitrary `Bifunctor` with indexed and co-indexed typeclass constraint `C`. */
//   def apply[F[-_, +_, -_, +_], J <: I, E1 >: E](pab: F[J, E1, A, B])(using C[J, E1, F]): F[J, E1, S, T]


//   //We want to hold the composition outside?
//   @alpha("compose")
//   final def <<<[C1 <: C, J <: I, E1, U, V](other: Optic[C1, J, E1, U, V, S, T]): Optic[C1, J, E | E1, U, V, A, B] = {
//     new Optic {
//       def apply[F[-_, +_, -_, +_], K <: J, E2 >: (E | E1)](pab: F[K, E2, A, B])(using bifunctor: C1[K, E2, F]): F[K, E2, U, V] =
//         other(self(pab))
//     }
//   }

//   @alpha("andThen")
//   final def >>>[C1 <: C, J <: I, E1, U, V](other: Optic[C1, J, E1, A, B, U, V]) =
//     other <<< self

// }

// type Lens[-I, +E, -S, +T, +A, -B] = Optic[IxCoixCartesian, I, E, S, T, A, B]

// object Lens {

//   /** Creates a polymorphic `Lens` from the `getter` and `setter`. */
//   def apply[S, T, A, B](
//     getter: S => A,
//     setter: S => B => T
//   ): Lens[Any, Nothing, S, T, A, B] =
//     new Optic {
//       def apply[F[-_, +_, -_, +_], J <: Any, E1 >: Nothing](
//         pab: F[J, E1, A, B]
//       )(using cartesian: IxCoixCartesian[J, E1, F]): F[J, E1, S, T] =
//         cartesian.dimap(cartesian.first(pab))(s => (getter(s), s), (b, s) => setter(s)(b))
//     }
// }

// type Prism[-I, +E, -S, +T, +A, -B] = Optic[IxCoixCocartesian, I, E, S, T, A, B]
// type Adapter[-I, +E, -S, +T, +A, -B] = Optic[IxCoixProfunctor, I, E, S, T, A, B]
// type Affine[-I, +E, -S, +T, +A, -B] = Optic[[I, E, F[_, _, _, _]] =>> IxCoixCartesian[I, E, F] & IxCoixCocartesian[I, E, F], I, E, S, T, A, B]
// type Traversal[-I, +E, -S, +T, +A, -B] = Optic[[I, E, F[_, _, _, _]] =>> IxCoixCartesian[I, E, F] & IxCoixCocartesian[I, E, F] & IxCoixWandering[I, E, F], I, E, S, T, A, B]
// type Getter[-I, +E, -S, +T, +A, -B] = Optic[[I, E, F[_, _, _, _]] =>> IxCoixBicontravariant[I, E, F] & IxCoixCartesian[I, E, F], I, E, S, T, A, B]

// // type Affine[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Cartesian[F] & Cocartesian[F], S, T, A, B]

// // type Lens[-S, +T, +A, -B] = Optic[Cartesian, S, T, A, B]
// // type Prism[-S, +T, +A, -B] = Optic[Cocartesian, S, T, A, B]
// // type Adapter[-S, +T, +A, -B] = Optic[Profunctor, S, T, A, B]
// // type Affine[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Cartesian[F] & Cocartesian[F], S, T, A, B]
// // type Traversal[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Monoidal[F] & Cartesian[F] & Cocartesian[F], S, T, A, B]
// // type Getter[-S, +T, +A, -B] = Optic[[F[_, _]] =>> Bicontravariant[F], S, T, A, B]

// object Optic {

//   // extension on [S, T, A, B](getter: Getter[S, T, A, B])  {
//   //   /** `get` */
//   //   inline def get: S => A =
//   //     getter(Forget(identity)).function
//   // }

//   extension on [S, T, A, B](lens: Getter[Any, Nothing, S, T, A, B])  {
//     /** `get` */
//     inline def get: S => A = ???
//    //   lens(Forget(identity)).function
//   }

//   // extension on [S, T, A, B](traversal: Traversal[S, T, A, B]) {
//   //   /** `modify` */
//   //   inline def modify(f: A => B): S => T =
//   //     traversal(f)
//   // }

// }

//             <- when we compose an indexed lens, we need 2 different profunctors
//Traversal[Int, ]

// type Indexed[F[-_, +_], -I, +A, -B] = I => F[A, B]

//a =

/** A optic which is indexed and co-indexed.
  *
  * We use a concrete implementation for the `carrier` of both the index and co-index like
  * the similar profunctor representations in `purescript` and `haskell` but we can't pull
  * off the same `trick` with currying `->` since polymorphic functions with typeclass bounds
  * are not functions in scala.
  *
  */
trait IndexedOptic[-C[_[_, _]], I <: Tuple, J <: Tuple, +E, -S, +T, +A, -B] { self =>

  def apply[C1 <: C, F[-_, +_], EE >: E, K <: I, L >: J](pab: K => Either[EE, (L, F[A, B])])(using C1[F]): Either[EE, (J, F[S, T])]

  final inline def <<<[C1 <: C, EE, K <: Tuple, U, V](
    other: IndexedOptic[C1, K, I, EE, U, V, S, T]
  ): IndexedOptic[C1, K, Tuple.Concat[I, J], E | EE, U, V, A, B] = ???

  /**
    * This allows us to produce multiple indicies which are then consumed later.
    */
  final inline def <<<[C1 <: C, EE, H <: Tuple, K <: Tuple, U, V](
    other: IndexedOptic[C1, K, H, EE, U, V, S, T]
  )(using I =:= Unit): IndexedOptic[C1, K, Tuple.Concat[H, J], E | EE, U, V, A, B] = ???

  // final inline def <<<[C1 <: C, EE, K <: J, L <: Tuple, U, V](
  //   other: IndexedOptic[C1, K, L, EE, U, V, S, T]
  // ): IndexedOptic[C1, L, Tuple.Concat[K, J], E | EE, U, V, A, B] = ???
    // new IndexedOptic {
    //   def apply[CC <: C1, F[-_, +_], EEE >: (E | EE)](pab: K => Either[EEE, (J, F[A, B])])(using cc: CC[F]) = {
    //     // val sizeK = constValue[Tuple.Size[K]]
    //     // val sizeL = constValue[Tuple.Size[L]]
    //     other(k => {
    //       self((i) => {
    //         pab(k).map {
    //           case (j, fab) =>
    //             (j, fab)
    //         }
    //       })(using cc)
    //     })(using cc)
    //     ???
    //   }
    // }


  // final inline def <<<[C1 <: C, EE, K <: Tuple, L <: Tuple, U, V](
  //   other: IndexedOptic[C1, K, L, EE, U, V, S, T]
  // )(using size: Tuple.Size[K]): IndexedOptic[C1, Tuple.Concat[K, I], Tuple.Concat[L, J], E | EE, U, V, A, B] =
  //   new IndexedOptic {
  //     def apply[CC <: C1, F[-_, +_], EEE >: (E | EE)](pab: Tuple.Concat[K, I] => Either[EEE, (Tuple.Concat[L, J], F[A, B])])(using cc: CC[F]) = {
  //       // val sizeK = constValue[Tuple.Size[K]]
  //       // val sizeL = constValue[Tuple.Size[L]]
  //       // other(k => {
  //       //   self((i) => {
  //       //     pab(k ++ i).map {
  //       //       case (concat, fab) =>
  //       //         (concat.drop(size).asInstanceOf[J], fab)
  //       //     }
  //       //   })(using cc).map {
  //       //     case (j, fab) =>
  //       //       pab(k ++ i).map {
  //       //         case (l, _) =>
  //       //           (l ++ j, fab)
  //       //       }
  //       //   }
  //       // })(using cc)
  //       // self(i => {
  //       //   pab(i).map {
  //       //     case (concat, fab) =>
  //       //       (concat.drop(size).asInstanceOf[J], fab)
  //       //   }
  //       // })

  //       ???
  //     }
  //   }



//   //We want to hold the composition outside?
//   @alpha("compose")
//   final def <<<[C1 <: C, J <: I, E1, U, V](other: Optic[C1, J, E1, U, V, S, T]): Optic[C1, J, E | E1, U, V, A, B] = {
//     new Optic {
//       def apply[F[-_, +_, -_, +_], K <: J, E2 >: (E | E1)](pab: F[K, E2, A, B])(using bifunctor: C1[K, E2, F]): F[K, E2, U, V] =
//         other(self(pab))
//     }
//   }

}

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

type IndexedLens[I <: Tuple, J <: Tuple, +E, -S, +T, +A, -B] = IndexedOptic[Cartesian, I, J, E, S, T, A, B]
type Optic[-C[_[_, _]], -S, +T, +A, -B] = IndexedOptic[C, Unit, Unit, Nothing, S, T, A, B]

object Main {

  val lens: IndexedLens[Unit, Unit, Nothing, Int, Int, Int, Int] = ???
  val traversal: IndexedLens[Unit, Int *: Unit, Nothing, Int, Int, Int, Int] = ???
  val lens2: IndexedLens[Int *: Unit, Unit, Nothing, Int, Int, Int, Int] = ???

  val x = lens2 <<< (lens <<< traversal)
  val y = lens2 <<< traversal

}






// trait IndexedCoindexedProfunctor[F[_, _, _, _]] {
//   def dimap[A, B, C, D, I, J](fab: F[I, J, A, B])(f: C => A, g: B => D): F[I, J, C, D]
// }

// trait Applicative[F[+_]]{
//   def pure[A](value: A): F[A]
//   def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
//   def map[A, B](fa: F[A])(f: A => B): F[B]
// }

// trait TraversableWithIndex[F[_, _]]

// trait IndexedCoindexedWander[F[_, _, _, _]] extends IndexedCoindexedProfunctor[F] {

//   def iwanderI[I, O, S, T, A, B, K, L, F[+_] : Applicative](
//     f: (I, A) => F[B], g: S => F[T]
//   ): IndexedCoindexedOptic[IndexedCoindexedWander, (I, O), O, K, L, S, T, A, B]

// }


// // trait IxCoixProfunctor[F[_, _, _, _]] {
// //   def dimap[A, B, C, D, I, J](fab: F[I, J, A, B])(f: C => A, g: B => D): F[I, J, C, D]
// // }



// // trait IxCoixTraversing[F[_, _, _, _]] extends IxCoixProfunctor[F] {

// //   // def iwanderI[I, A, B, S, T, O, F[_] : Applicative](f: (I, A) => F[B], g: S => F[T]): IxCoixOptic[Nothing, ()]

// // }
