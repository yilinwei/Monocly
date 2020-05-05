package optics.mono

import scala.quoted._

object Reify {

  //a.

  def impl[S: Type, T: Type](getter: Expr[S => T])(using qctx: QuoteContext): Expr[Lens[S, T]] = {

    import qctx.tasty._
    import util._

    //


    //a.b?.c.*.d.e
    //_.check(a.check(b.c))

    // obj.copy(a = obj.a.copy(b = a.b.copy(c = v)))
    // def setterBody(obj: Term, value: Term, parts: List[String]): Term = {
    //   // o.copy(field = value)
    //   def helper(obj: Term, value: Term, field: String): Term =
    //     Select.overloaded(obj, "copy", Nil, NamedArg(field, value) :: Nil)

    //   parts match {
    //     case field :: Nil => helper(obj, value, field)
    //     case field :: parts =>
    //       helper(obj, setterBody(Select.unique(obj, field), value, parts), field)
    //   }
    // }

    // object Path {
    //   private def recur(tree: Term, selects: List[String]): Option[(Term, List[String])] = tree match {
    //     case Ident(_) if selects.nonEmpty => Some((tree, selects))
    //     case Select(qual, name) => recur(qual, name :: selects)
    //     case _ => None
    //   }

    //   def unapply(t: Term): Option[(Term, List[String])] = recur(t, Nil)
    // }

    object Function {
      def unapply(t: Term): Option[(List[ValDef], Term)] = t match {
        case Inlined(None, Nil, Lambda(params, body)) => Some((params, body))
        case _ => None
      }
    }


    val fromType = typeOf[S]

    if (fromType.classSymbol.flatMap(cls => if (cls.flags.is(Flags.Case)) Some(true) else None).isEmpty) {
      qctx.error("Only support generation for case classes")
      return '{???}
    }

    // exception: getter.unseal.underlyingArgument
    getter.unseal match {
      // base case
      case Function(param :: Nil, Select(Ident(_), name)) =>
        '{
          // val setter = (t: T) => (s: S) => ${ setterBody(('s).unseal, ('t).unseal, parts).seal.cast[S] }
          // Lens.apply($getter, setter)
          ???
        }
      // recurse case
      case Function(param :: Nil, Select(sel @ Select(Ident(_), inner), outer)) =>
        if(!fromType.classSymbol.get.field(inner).flags.is(Flags.Case)) {
          qctx.error(s"member `$inner` is not a case class")
        }
        '{
          // val setter = (t: T) => (s: S) => ${ setterBody(('s).unseal, ('t).unseal, parts).seal.cast[S] }
          // Lens.apply($getter, setter)
          ???
        }
      case err =>
        qctx.error(s"Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`, $err")
        '{???}
    }
  }

  def apply[S] = new MkReify[S]

  class MkReify[S] {
    inline def apply[T](inline get: (S => T)): Lens[S, T] = ${ Reify.impl('get) }
  }

}
