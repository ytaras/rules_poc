import Rule._
import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by ytaras on 4/9/16.
  */
class RulesCombinatorsSpec extends Specification with ScalaCheck {
  val pMapCombine = Prop.forAll { m: Map[String, Int] =>
    val ruleMap = m.map { case (k, v) => k -> v.pureRule[Unit, Unit, Unit] }
    val mapRule = Rule.toMap(ruleMap)
    mapRule.unsafe(???, ???, ???) === m
  }

  val pTraverse = Prop.forAll { l: List[Int] =>
    val rulesList = l.map(_.pureRule[Unit, Unit, Unit])
    val traversed = Rule.sequence(rulesList)

    traversed.unsafe(???, ???, ???) === l
  }

  def is =
    s2"""
       Can combine into map: $pMapCombine
       Can traverse: $pTraverse
    """

}

