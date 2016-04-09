import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
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

  def toMap[S, I, P, K, V](m: Map[K, Rule[S, I, P, V]]): Rule[S, I, P, Map[K, V]] = {
    val rulesSeq = m.map { case (k, r) => r.map(k -> _) }
    sequence(rulesSeq).map(_.toMap)
  }

  def sequence[S, I, P, V, M[+ _] <: TraversableOnce[_]](s: M[Rule[S, I, P, V]])
                                                        (implicit cbf: CanBuildFrom[M[Rule[S, I, P, V]], V, M[V]]): Rule[S, I, P, M[V]] = {
    type MB = mutable.Builder[V, M[V]]
    type Ru = Rule[S, I, P, V]
    type RuMB = Rule[S, I, P, MB]
    val builder = cbf.apply(s)
    val acc = builder.pureRule[S, I, P]
    def append(acc: RuMB, item: Ru): RuMB =
      acc.addWith[V, MB](_ += _)(item)
    val r = s.asInstanceOf[M[Ru]].foldLeft(acc) { (o, t) =>
      // TODO - Figure out why i should cast
      append(o, t.asInstanceOf[Ru])
    }
    r.map(_.result())
  }

  def pure[I, S, P, O](v: => O): Rule[I, S, P, O] =
    fromPure((_, _, _) => v)

  def fromPure[A, B, C, D](f: (=> A, => B, => C) => D): Rule[A, B, C, D] = Rule {
    (a, b, c) => Try {
      f(a, b, c)
    }
  }

  implicit class FunctionalRuleSyntax[I, S, P, O, B](r: Rule[I, S, P, O => B]) {
    def ap(b: Rule[I, S, P, O]): Rule[I, S, P, B] =
      r.flatMap { f => b.map(f) }
  }

  implicit class FunctionSyntax[V, O](f: V => O) {
    def inputRule[S, P]: Rule[V, S, P, O] =
      fromPure((i, _, _) => f(i))

    def stateRule[I, P]: Rule[I, V, P, O] =
      fromPure((_, s, _) => f(s))

    def paramRule[I, S]: Rule[I, S, V, O] =
      fromPure((_, _, p) => f(p))
  }

  implicit class ValueSyntax[V](v: => V) {
    def pureRule[I, S, P]: Rule[I, S, P, V] =
      fromPure((_, _, _) => v)

    def tryRule[I, S, P, O](implicit ev: V <:< Try[O]): Rule[I, S, P, O] = Rule((_, _, _) => ev(v))
  }

}


