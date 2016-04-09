import scala.util.Try

/**
  * Created by ytaras on 4/9/16.
  */

case class Rule[In, St, Params, +Out](run: (=> In, => St, => Params) => Try[Out]) {
  def unsafe(a: => In, b: => St, c: => Params) = run(a, b, c).get

  def addWith[OOut, FOut](f: (Out, OOut) => FOut)(other: Rule[In, St, Params, OOut]) =
    flatMap(result => other.map(f(result, _)))

  def map[BOut](f: Out => BOut): Rule[In, St, Params, BOut] =
    Rule { (a, b, c) => run(a, b, c).map(f) }

  def flatMap[BOut](f: Out => Rule[In, St, Params, BOut]): Rule[In, St, Params, BOut] = Rule {
    (a, b, c) => for {
      r1 <- run(a, b, c)
      newRule = f(r1)
      newRes <- newRule.run(a, b, c)
    } yield newRes
  }

}

object Rule {
  // TODO Deal with type params
  def fromInput[I, S, P, O](f: I => O): Rule[I, S, P, O] =
    fromPure((i, _, _) => f(i))

  def fromPure[A, B, C, D](f: (=> A, => B, => C) => D): Rule[A, B, C, D] = Rule {
    (a, b, c) => Try {
      f(a, b, c)
    }
  }

  def fromState[I, S, P, O](f: S => O): Rule[I, S, P, O] =
    fromPure((_, s, _) => f(s))

  def fromParam[I, S, P, O](f: P => O): Rule[I, S, P, O] =
    fromPure((_, _, p) => f(p))

  def toMap[S, I, P, K, V](m: scala.collection.immutable.Map[K, Rule[S, I, P, V]]): Rule[S, I, P, Map[K, V]] = {
    val rulesSeq: scala.collection.immutable.Iterable[Rule[S, I, P, (K, V)]] =
      m.map { case (k, r) => r.map(k -> _) }
    val emptyMapRule: Rule[S, I, P, Map[K, V]] = Rule.pure(Map.empty)
    rulesSeq.foldLeft(emptyMapRule) { (rM, rT) =>
      rM.addWith[(K, V), Map[K, V]]((m, v) => m + v)(rT)
    }
  }

  def pure[I, S, P, O](v: => O): Rule[I, S, P, O] =
    fromPure((_, _, _) => v)

  implicit class FunctionalRuleSyntax[I, S, P, O, B](r: Rule[I, S, P, O => B]) {
    def ap(b: Rule[I, S, P, O]): Rule[I, S, P, B] =
      r.flatMap { f => b.map(f) }
  }
}


