import org.scalacheck.Prop
import org.specs2.{ScalaCheck, Specification}

/**
  * Created by ytaras on 4/9/16.
  */
class RulesCombinatorsSpec extends Specification with ScalaCheck {
  val pMapCombine = Prop.forAll { m: scala.collection.immutable.Map[String, Int] =>
    val rulesMap = m.map { case (k, v) => k -> Rule.pure[Nothing, Nothing, Nothing, Int](v) }
    Rule.toMap[Nothing, Nothing, Nothing, String, Int](rulesMap).unsafe(???, ???, ???) === m
  }

  def is =
    s2"""
       Can combine into map: $pMapCombine
    """

}
