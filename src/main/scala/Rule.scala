/**
  * Created by ytaras on 4/9/16.
  */

case class Rule[-In, -St, -Params, +Out](run: (In, St, Params) => Out)

object Rule {
  def fromInput[A, B](f: A => B): Rule[A, Any, Any, B] =
    Rule { (a, _, _) => f(a) }
  def fromState[A, B](f: A => B): Rule[Any, A, Any, B] =
    Rule { (_, a, _) => f(a) }
  def fromParam[A, B](f: A => B): Rule[Any, Any, A, B] =
    Rule { (_, _, a) => f(a) }
}

