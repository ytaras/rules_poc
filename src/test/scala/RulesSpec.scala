import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by ytaras on 4/9/16.
  */
class RulesSpec extends Specification with ScalaCheck { def is = s2"""
     Rules use input: $pInput
     Rules use state: $pState
     Rules use params: $pParam
     Rules can use everything: $pAll
    """

  // TODO - Reduce boilderplate
  val pInput = Prop.forAll { (a: Int, b: Int) =>
    Rule.fromInput[Int, Int] { _ + a }.run(b, None, None) === a + b
  }
  val pState = Prop.forAll { (a: Int, b: Int) =>
    Rule.fromState[Int, Int] { _ + a}.run(None, b, None) === a + b
  }
  val pParam = Prop.forAll { (a: Int, b: Int) =>
    Rule.fromParam[Int, Int] { _ + a}.run(None, None, b) === a + b
  }
  val pAll = Prop.forAll { (a: Int, b: Int, c: Int) =>
    Rule[Int, Int, Int, Int] { _ + _ + _}.run(a, b, c) === a + b + c
  }

}
