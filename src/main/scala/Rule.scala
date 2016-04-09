/**
  * Created by ytaras on 4/9/16.
  */

case class Rule[In, St, Params, +Out](run: (=> In, => St, => Params) => Out) {
  def map[BOut](f: Out => BOut): Rule[In, St, Params, BOut] =
    Rule { (a, b, c) => f(run(a, b, c)) }

  def flatMap[BOut](f: Out => Rule[In, St, Params, BOut]): Rule[In, St, Params, BOut] = Rule {
    (a, b, c) => f(run(a, b, c)).run(a, b, c)
  }

}

object Rule {
  def fromInput[A, B](f: A => B): Rule[A, Any, Any, B] =
    Rule { (a, _, _) => f(a) }
  def fromState[A, B](f: A => B): Rule[Any, A, Any, B] =
    Rule { (_, a, _) => f(a) }
  def fromParam[A, B](f: A => B): Rule[Any, Any, A, B] =
    Rule { (_, _, a) => f(a) }

  def pure[A](v: => A): Rule[Any, Any, Any, A] =
    Rule { (_, _, _) => v }
}

