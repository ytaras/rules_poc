import Rule._
import cats._
import cats.implicits._
import cats.laws.discipline.CartesianTests.Isomorphisms
import cats.laws.discipline.MonadTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.Specification
import org.specs2.specification.core.SpecStructure
import org.typelevel.discipline.specs2.Discipline

import scala.util.Failure

/**
  * Created by ytaras on 4/11/16.
  */
class RulesTypeclassesSpec extends Specification with Discipline {
  val goodFloatIntRuleGen = for {
    (coefI, coefS, coefP, total) <- arbitrary[(Float, Float, Float, Float)]
  } yield Rule.fromPure[Int, Int, Int, Int => Float] { (i, s, p) =>
    total * _ + i * coefI + s * coefS + p * coefP
  }
  val goodFloatRuleGen = for {
    (coefI, coefS, coefP) <- arbitrary[(Float, Float, Float)]
  } yield Rule.fromPure[Int, Int, Int, Float] { (i, s, p) => i * coefI + s * coefS + p * coefP }
  val goodIntStringRuleGen = for {
    str <- arbitrary[String]
  } yield Rule.fromPure[Int, Int, Int, Int => String] { (i, s, p) =>
    x => s"$str $i $s $p $x"
  }
  val goodFloatStringRuleGen = for {
    str <- arbitrary[String]
  } yield Rule.fromPure[Int, Int, Int, Float => String] { (i, s, p) =>
    x => s"$str $i $s $p $x"
  }
  val goodStringRuleGen = for {
    str <- arbitrary[String]
  } yield Rule.fromPure[Int, Int, Int, String] { (i, s, p) =>
    s"$str $i $s $p"
  }
  val goodIntRuleGen = for {
    (coefI, coefS, coefP) <- arbitrary[(Int, Int, Int)]
  } yield Rule.fromPure[Int, Int, Int, Int] { (i, s, p) => i * coefI + s * coefS + p * coefP }
  implicit val arbFA: Arbitrary[Rule[Int, Int, Int, Int]] =
    Arbitrary(Gen.oneOf(goodIntRuleGen, badGen[Int, Int, Int, Int]))
  val e1 = checkAll("Rule", MonadTests[Rule[Int, Int, Int, ?]].monad[Int, Float, String])

  override def is: SpecStructure =
    s2"""
       | Just for fun, no needs to have Cats
       | Rule[Int, Int, Map[String, Int], ?] forms a Monad $e1
      """.stripMargin

  implicit def ruleMonad[In, St, P] = new Monad[Rule[In, St, P, ?]] {
    override def pure[A](x: A): Rule[In, St, P, A] = x.pureRule[In, St, P]

    override def flatMap[A, B](fa: Rule[In, St, P, A])(f: (A) => Rule[In, St, P, B]) =
      fa.flatMap(f)
  }

  implicit def eqRule[A: Eq] = Eq.instance[Rule[Int, Int, Int, A]] {
    (rule1, rule2) =>
      val samples = Gen.listOfN(10, arbitrary[(Int, Int, Int)]).sample.get
      samples.forall { case (i, s, p) =>
        Eq.eqv(
          rule1.run(i, s, p).toOption,
          rule2.run(i, s, p).toOption)
      }
  }

  implicit def eqTuple[A: Eq, B: Eq, C: Eq] = Eq.instance[(A, B, C)] {
    (t1, t2) =>
      import Eq.eqv
      eqv(t1._1, t2._1) &&
        eqv(t1._2, t2._2) &&
        eqv(t1._3, t2._3)
  }

  implicit def arbFB: Arbitrary[Rule[Int, Int, Int, String]] =
    Arbitrary(Gen.oneOf(goodStringRuleGen, badGen[Int, Int, Int, String]))

  implicit def arbFc: Arbitrary[Rule[Int, Int, Int, Float]] =
    Arbitrary(Gen.oneOf(goodFloatRuleGen, badGen[Int, Int, Int, Float]))

  implicit def arbFaToFc: Arbitrary[Rule[Int, Int, Int, Int => Float]] =
    Arbitrary(Gen.oneOf(goodFloatIntRuleGen, badGen[Int, Int, Int, Int => Float]))

  def badGen[A, B, C, D] = Gen.const(Rule[A, B, C, D] { (_, _, _) => Failure(new RuntimeException) })

  implicit def arbFaToFb: Arbitrary[Rule[Int, Int, Int, Float => String]] =
    Arbitrary(Gen.oneOf(goodFloatStringRuleGen, badGen[Int, Int, Int, Float => String]))

  implicit def iso: Isomorphisms[Rule[Int, Int, Int, ?]] =
    Isomorphisms.invariant[Rule[Int, Int, Int, ?]]

  implicit def arbFbToFc: Arbitrary[Rule[Int, Int, Int, Int => String]] =
    Arbitrary(Gen.oneOf(goodIntStringRuleGen, badGen[Int, Int, Int, Int => String]))
}
