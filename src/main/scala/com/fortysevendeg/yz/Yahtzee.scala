package com.fortysevendeg.yz

object Yahtzee {

  sealed trait Die
  case object One extends Die
  case object Two extends Die
  case object Three extends Die
  case object Four extends Die
  case object Five extends Die
  case object Six extends Die

  sealed trait Score { val value: Int }
  case object Yahtzee extends Score { val value = 5 }
  case object Straight extends Score { val value = 4 }
  case object FullHouse extends Score { val value = 3 }
  case object FourOfAKind extends Score { val value = 2 }
  case object ThreeOfAKind extends Score { val value = 1 }
  case object NoScore extends Score { val value = 0 }

  case class Hand(p1: Die, p2: Die, p3: Die, p4: Die, p5: Die) {
    val score = {

      val diceList = List(p1, p2, p3, p4, p5)
      val diceSet = diceList.toSet

      val grouped: Map[Die, Int] = diceList.groupBy(identity).map{case (k, v) => (k, v.length)}
      val valueSet = grouped.values.toSet

      if(diceList.distinct.size == 1) Yahtzee
      else if(diceSet == Set(One, Two, Three, Four, Five) || diceSet == Set(Two, Three, Four, Five, Six)) Straight
      else if(valueSet.contains(4)) FourOfAKind
      else if(valueSet == Set(3, 2)) FullHouse
      else if(valueSet == Set(3, 1)) ThreeOfAKind
      else NoScore
    }
  }

  def winner(h1: Hand, h2: Hand): Hand = {
    if(h1.score.value >= h2.score.value) h1 else h2
  }
}
