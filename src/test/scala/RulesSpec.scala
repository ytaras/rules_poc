import Rule._
import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}

import scala.util.Try

/**
  * Created by ytaras on 4/9/16.
  */
class RulesSpec extends Specification with ScalaCheck {

  val pInput = Prop.forAll { (a: Int, b: Int) =>
    addLambda(a).inputRule.unsafe(b, notRead, notRead) === a + b
  }
  val pState = Prop.forAll { (a: Int, b: Int) =>
    addLambda(a).stateRule.unsafe(notRead, b, notRead) === a + b
  }
  val pParam = Prop.forAll { (a: Int, b: Int) =>
    addLambda(a).paramRule.unsafe(notRead, notRead, b) === a + b
  }
  val pAll = Prop.forAll { (a: Int, b: Int, c: Int) =>
    Rule.fromPure[Int, Int, Int, Int] {
      _ + _ + _
    }.unsafe(a, b, c) === a + b + c
  }
  val pPure = Prop.forAll {
    (a: Int) => Rule.pure(a).unsafe(notRead, notRead, notRead) === a
  }
  val pMap = Prop.forAll { (a: Int, b: Int) =>
    a.pureRule[Unit, Unit, Unit]
      .map(_ * b).unsafe(notRead, notRead, notRead) === a * b
  }
  val pFlatMap = Prop.forAll { (a: Int, b: Int) =>
    val r = for {
      in1 <- a.pureRule[Unit, Unit, Unit]
      in2 <- b.pureRule[Unit, Unit, Unit]
    } yield in1 * in2
    r.unsafe(notRead, notRead, notRead) === a * b
  }
  val pAp = Prop.forAll { (a: Int, b: Int) =>
    addLambda(a).pureRule[Unit, Unit, Unit].ap(b.pureRule)
      .unsafe(notRead, notRead, notRead) === a + b
  }

  val pTry = Prop.forAll { (a: Option[Int]) =>
    val t = Try {
      a.get
    }
    t.tryRule.run(???, ???, ???) === t
  }

  def addLambda(a: Int) = (b: Int) => a + b

  def is =
    s2"""
     Rules use input: $pInput
     Rules use state: $pState
     Rules use params: $pParam
     Rules can use everything: $pAll
     Rules can be build from Try: $pTry

     Rule is a monad:
     Can return: $pPure
     Can map: $pMap
     Can flatMap: $pFlatMap
     Can ap: $pAp
    """

  //noinspection NotImplementedCode
  def notRead = ???

}
