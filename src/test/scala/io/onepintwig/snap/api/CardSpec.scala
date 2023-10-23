package io.onepintwig.snap.api

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CardSpec extends AnyFlatSpec with Matchers {

  "snapCheck" should "snap if suit matches" in {
    val card1 = Card(Spades, 1)
    val card2 = Card(Spades, 2)
    card1.snapCheck(card2, snapValue = false, snapSuit = true) shouldBe true
    card1.snapCheck(card2, snapValue = true, snapSuit = true) shouldBe true
    card1.snapCheck(card2, snapValue = true, snapSuit = false) shouldBe false
    card1.snapCheck(card2, snapValue = false, snapSuit = false) shouldBe false
  }

  it should "snap if value matches" in {
    val card1 = Card(Spades, 1)
    val card2 = Card(Hearts, 1)
    card1.snapCheck(card2, snapValue = false, snapSuit = true) shouldBe false
    card1.snapCheck(card2, snapValue = true, snapSuit = true) shouldBe true
    card1.snapCheck(card2, snapValue = true, snapSuit = false) shouldBe true
    card1.snapCheck(card2, snapValue = false, snapSuit = false) shouldBe false
  }

  it should "snap if value and suit matches" in {
    val card1 = Card(Spades, 1)
    val card2 = Card(Spades, 1)
    card1.snapCheck(card2, snapValue = false, snapSuit = true) shouldBe true
    card1.snapCheck(card2, snapValue = true, snapSuit = true) shouldBe true
    card1.snapCheck(card2, snapValue = true, snapSuit = false) shouldBe true
    card1.snapCheck(card2, snapValue = false, snapSuit = false) shouldBe false
  }

  it should "never snap if nothing matches" in {
    val card1 = Card(Spades, 1)
    val card2 = Card(Hearts, 2)
    card1.snapCheck(card2, snapValue = false, snapSuit = true) shouldBe false
    card1.snapCheck(card2, snapValue = true, snapSuit = true) shouldBe false
    card1.snapCheck(card2, snapValue = true, snapSuit = false) shouldBe false
    card1.snapCheck(card2, snapValue = false, snapSuit = false) shouldBe false
  }

}
