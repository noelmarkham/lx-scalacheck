package com.fortysevendeg.yz

import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._

object ExampleProperties extends Properties("Example Properties") {

  property("String has non-negative length") = forAll { s: String =>
    s.length >= 0
  }

  property("Failing: List concatenation") = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.length < (l1 ::: l2).length
  }

  property("Fixed: List concatenation") = forAll { (l1: List[Int], l2: List[Int]) =>
    (l1.length > 0 && l2.length > 0) ==> {
      l1.length < (l1 ::: l2).length
    }
  }

  def brokenReverse[X](xs: List[X]): List[X] = if (xs.length > 4) xs else xs.reverse

  property("Failing: Broken reverse") = forAll { (xs: List[Int]) => xs.length > 0 ==>
    (xs.last == brokenReverse(xs).head)
  }

  property("Failing: Math.abs") = forAll { x: Int =>
    Math.abs(x) >= 0
  }

  property("Fixed: Math.abs") = forAll(Gen.posNum[Int]) { x =>
    Math.abs(x) >= 0
  }

  property("Failing: Three positive integers") = forAll { (i1: Int, i2: Int, i3: Int) =>
    (i1 > 0 && i2 > 0 && i3 > 0) ==> {

      passed
    }
  }

  property("Fixed: Three positive integers") = forAll(posNum[Int], posNum[Int], posNum[Int]) { (i1, i2, i3) =>
    passed
  }

  property("Failing: Generators without labels") = forAll { (i: Int, m: Map[Int, String]) =>
    m.get(i).isDefined
  }

  property("Failing: Generators with labels") = forAll("Index" |: arbitrary[Int], "Lookup database" |: arbitrary[Map[Int, String]]) { (i, m) =>
    m.get(i).isDefined
  }

  property("Failing: Properties without labels") = forAllNoShrink { (i: Int, j: Int) =>
    val (max, min) = (i max j, i min j)
    val (maxSq, minSq) = (max * max, min * min)

    minSq <= maxSq
  }

  property("Failing: Properties with labels") = forAllNoShrink { (i: Int, j: Int) =>
    val (max, min) = (i max j, i min j)
    val (maxSq, minSq) = (max * max, min * min)

    s"[min: $min, square: $minSq], [max: $max, square: $maxSq]" |:
      (minSq <= maxSq)
  }

  import ExampleTypes._

  property("Using created arbitrary") = forAll { r: Record =>
    passed
  }
}

object ExampleTypes {

  def cappedString: Gen[String] = for {
    c <- alphaUpperChar
    s <- listOf(alphaLowerChar)
  } yield (c :: s).mkString

  case class Record(s: String)

  val genRecord: Gen[Record] = alphaStr.map(Record.apply)

  implicit val arbRecord: Arbitrary[Record] = Arbitrary(genRecord)
}
