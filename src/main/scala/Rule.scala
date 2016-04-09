import scala.util.Try

/**
  * Created by ytaras on 4/9/16.
  */

case class Rule[In, St, Params, +Out](run: (=> In, => St, => Params) => Try[Out]) {
  def unsafe(a: => In, b: => St, c: => Params) = run(a, b, c).get
  def map[BOut](f: Out => BOut): Rule[In, St, Params, BOut] =
    Rule { (a, b, c) => run(a, b, c).map(f) }

  def flatMap[BOut](f: Out => Rule[In, St, Params, BOut]): Rule[In, St, Params, BOut] = Rule {
    (a, b, c) => run(a, b, c).map(f).flatMap(_.run(a, b, c))
  }

}

object Rule {
  def fromInput[A, B](f: A => B): Rule[A, Any, Any, B] =
    Rule { (a, _, _) => Try {
      f(a)
    }
    }
  def fromState[A, B](f: A => B): Rule[Any, A, Any, B] =
    Rule { (_, a, _) => Try {
      f(a)
    }
    }
  def fromParam[A, B](f: A => B): Rule[Any, Any, A, B] =
    Rule { (_, _, a) => Try {
      f(a)
    }
    }

  def pure[A](v: => A): Rule[Any, Any, Any, A] =
    Rule { (_, _, _) => Try {
      v
    }
    }

  def fromPure[A, B, C, D](f: (A, B, C) => D): Rule[A, B, C, D] = Rule {
    (a, b, c) => Try {
      f(a, b, c)
    }
  }


  implicit class FunctionalRuleSyntax[A, B](r: Rule[Any, Any, Any, A => B]) {
    def ap(b: Rule[Any, Any, Any, A]): Rule[Any, Any, Any, B] =
      r.flatMap { f => b.map(f) }
  }
}


