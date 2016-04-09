import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by ytaras on 4/9/16.
  */
class RulesSpec extends Specification with ScalaCheck {
  val pInput = Prop.forAll { (a: Int, b: Int) =>
    Rule.fromInput[Int, Int] {
      _ + a
    }.run(b, notRead, notRead) === a + b
  }
  val pState = Prop.forAll { (a: Int, b: Int) =>
    Rule.fromState[Int, Int] {
      _ + a
    }.run(notRead, b, notRead) === a + b
  }
  val pParam = Prop.forAll { (a: Int, b: Int) =>
    Rule.fromParam[Int, Int] {
      _ + a
    }.run(notRead, notRead, b) === a + b
  }
  val pAll = Prop.forAll { (a: Int, b: Int, c: Int) =>
    Rule[Int, Int, Int, Int] {
      _ + _ + _
    }.run(a, b, c) === a + b + c
  }
  val pPure = Prop.forAll {
    (a: Int) => Rule.pure(a).run(notRead, notRead, notRead) === a
  }
  val pMap = Prop.forAll { (a: Int, b: Int) =>
    Rule.pure(a)
      .map(_ * b).run(notRead, notRead, notRead) === a * b
  }
  val pFlatMap = Prop.forAll { (a: Int, b: Int) =>
    val r = for {
      in1 <- Rule.pure(a)
      in2 <- Rule.pure(b)
    } yield in1 * in2
    r.run(notRead, notRead, notRead) === a * b
  }

  val pAp = Prop.forAll { (a: Int, b: Int) =>
    Rule.pure[Int => Int](_ + a).ap(Rule.pure(b))
      .run(notRead, notRead, notRead) === a + b
  }

  def is =
    s2"""
     Rules use input: $pInput
     Rules use state: $pState
     Rules use params: $pParam
     Rules can use everything: $pAll

     Rule is a monad:
     Can return: $pPure
     Can map: $pMap
     Can flatMap: $pFlatMap
     Can ap: $pAp
    """

  //noinspection NotImplementedCode
  def notRead = ???

}
