package com.fortysevendeg.yz

import org.scalacheck.Prop._
import org.scalacheck.Properties
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary

import Yahtzee._

object YahtzeeProperties extends Properties("Yahtzee Properties") with YahtzeeTestDomain {

  property("Winning hand is chosen properly") = forAll(chooseNum[Int](1, orderedGenerators.length - 1)) { idx =>

    val (winningHandGenerator, losingHandGenerator) = {
      if(idx == 1) (genYahtzee, oneOf(genStraight, genFullHouse, genFourOfAKind, genThreeOfAKind))
      else if (idx == orderedGenerators.length - 1) (oneOf(genYahtzee, genStraight, genFullHouse, genFourOfAKind), genThreeOfAKind)
      else {
        val (wg1 :: wg2 :: wgn, lg1 :: lg2 :: lgn) = orderedGenerators.splitAt(idx)
        (oneOf(wg1, wg2, wgn: _*), oneOf(lg1, lg2, lgn: _*))
      }
    }

    forAll(winningHandGenerator, losingHandGenerator) { (winningHand, losingHand) =>
      (winner(winningHand, losingHand) ?= winningHand) &&
      (winner(losingHand, winningHand) ?= winningHand)
    }
  }

  property("Winning hand is chosen properly with stats") = forAll(chooseNum[Int](1, orderedGenerators.length - 1)) { idx =>
    val (winningHandGenerator, losingHandGenerator) = {
      if(idx == 1) (genYahtzee, oneOf(genStraight, genFullHouse, genFourOfAKind, genThreeOfAKind))
      else if (idx == orderedGenerators.length - 1) (oneOf(genYahtzee, genStraight, genFullHouse, genFourOfAKind), genThreeOfAKind)
      else {
        val (wg1 :: wg2 :: wgn, lg1 :: lg2 :: lgn) = orderedGenerators.splitAt(idx)
        (oneOf(wg1, wg2, wgn: _*), oneOf(lg1, lg2, lgn: _*))
      }
    }

    forAll(winningHandGenerator, losingHandGenerator) { (winningHand, losingHand) =>
      collect(s"${winningHand.score} vs ${losingHand.score}") {
        (winner(winningHand, losingHand) ?= winningHand) &&
        (winner(losingHand, winningHand) ?= winningHand)
      }
    }
  }

}

trait YahtzeeTestDomain {

  val allDice: List[Die] = List(One, Two, Three, Four, Five, Six)

  val genYahtzee: Gen[Hand] = oneOf(allDice).map(d => Hand(d, d, d, d, d))

  val genStraight: Gen[Hand] = {
    val hand1 = List(One, Two, Three, Four, Five)
    val hand2 = List(Two, Three, Four, Five, Six)

    for {
      dice <- oneOf(hand1, hand2)
      p1 <- oneOf(dice)
      next1 = dice diff List(p1)
      p2 <- oneOf(next1)
      next2 = next1 diff List(p2)
      p3 <- oneOf(next2)
      next3 = next2 diff List(p3)
      p4 <- oneOf(next3)
      next4 = next3 diff List(p4)
      p5 <- oneOf(next4)
    } yield Hand(p1, p2, p3, p4, p5)
  }

  val genFourOfAKind: Gen[Hand] = for {
    d1 <- oneOf(allDice)
    d2 <- oneOf(allDice diff List(d1))
  } yield Hand(d1, d1, d1, d1, d2)

  val genThreeOfAKind: Gen[Hand] = for {
    d1 <- oneOf(allDice)
    d2 <- oneOf(allDice diff List(d1))
    d3 <- oneOf(allDice diff List(d1, d2))
  } yield Hand(d1, d1, d1, d2, d3)

  val genFullHouse: Gen[Hand] = for {
    d1 <- oneOf(allDice)
    d2 <- oneOf(allDice diff List(d1))
  } yield Hand(d1, d1, d1, d2, d2)

  val orderedGenerators: List[Gen[Hand]] = List(
    genYahtzee,
    genStraight,
    genFullHouse,
    genFourOfAKind,
    genThreeOfAKind
  )
}

object YahtzeeTestProperties extends Properties("Testing Yahtzee Generators") with YahtzeeTestDomain {

  property("Generating Yahtzee") = forAll(genYahtzee) { hand =>
    import hand._
    val diceSet = Set(p1, p2, p3, p4, p5)

    s"All dice should have the same value: $hand" |: diceSet.size == 1
  }

  property("Generating straight") = forAll(genStraight) { hand =>
    import hand._
    val diceSet = Set(p1, p2, p3, p4, p5)

    val possibleStraight1: Set[Die] = Set(One, Two, Three, Four, Five)
    val possibleStraight2: Set[Die] = Set(Two, Three, Four, Five, Six)

    (diceSet ?= possibleStraight1) || (diceSet ?= possibleStraight2)
  }

  property("Generating 4-o-a-k") = forAll(genFourOfAKind) { hand =>
    import hand._
    val diceList = List(p1, p2, p3, p4, p5)

    val grouped: Map[Die, Int] = diceList.groupBy(identity).map{case (k, v) => (k, v.length)}

    val valueSet = grouped.values.toSet

    valueSet ?= Set(1, 4)
  }

  property("Generating 3-o-a-k") = forAll(genThreeOfAKind) { hand =>
    import hand._
    val diceList = List(p1, p2, p3, p4, p5)

    val grouped: Map[Die, Int] = diceList.groupBy(identity).map{case (k, v) => (k, v.length)}

    val valueSet = grouped.values.toSet

    valueSet ?= Set(1, 3)
  }

  property("Generating full house") = forAll(genFullHouse) { hand =>
    import hand._
    val diceList = List(p1, p2, p3, p4, p5)

    val grouped: Map[Die, Int] = diceList.groupBy(identity).map{case (k, v) => (k, v.length)}

    val valueSet = grouped.values.toSet

    valueSet ?= Set(2, 3)
  }
}

