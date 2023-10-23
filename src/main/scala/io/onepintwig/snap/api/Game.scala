package io.onepintwig.snap.api

import scala.util.Random

/** This class contains all the state for the game */
private [snap] case class Game(
                 remainingCards: Seq[Card],
                 player1Stack: Seq[Card],
                 player2Stack: Seq[Card],
                 player1Score: Int,
                 player2Score: Int,
                 snapOnSuit: Boolean,
                 snapOnValue: Boolean
               ) {

  /**
   * Turns a card from the deck onto a players hand
   *
   * @param player determines which player draws a card
   * @return The updated [[Game]] state
   */
  def drawCard(player: Player): Game = {
    val newCard = remainingCards.head
    player match {
      case PlayerOne =>
        this.copy(remainingCards = remainingCards.tail, player1Stack = player1Stack :+ newCard)
      case PlayerTwo =>
        this.copy(remainingCards = remainingCards.tail, player2Stack = player2Stack :+ newCard)
    }
  }

  /**
   * Bank all played cards as points to the specified winner
   *
   * @param winner determines which player wins the round
   * @return The updated [[Game]] state
   */
  def updateWinner(winner: Player): Game = {
    val points = player1Stack.size + player2Stack.size
    val updateScores = winner match {
      case PlayerOne =>
        this.copy(player1Score = player1Score + points)
      case PlayerTwo =>
        this.copy(player2Score = player2Score + points)
    }
    //Clear stacks after winning
    updateScores.copy(player1Stack = Seq.empty, player2Stack = Seq.empty)
  }

  /**
   * Checks whether the current state has a snap
   *
   * @return A Boolean if there is currently a snap
   */
  def checkSnap: Boolean =
    if (player1Stack.isEmpty || player2Stack.isEmpty)
      false
    else player1Stack.last.snapCheck(player2Stack.last, snapOnValue, snapOnSuit)

}

case object Game {
  //Utility method for initialising a game
  def init(decks: Int, snapOnSuit: Boolean, snapOnValue: Boolean): Game = {
    val allCards: Seq[Card] = Random.shuffle {
      for {
        _ <- 1 to decks
        value <- 2 to 14
        suit <- Seq(Hearts, Diamonds, Clubs, Spades)
      } yield Card(suit, value)
    }
    Game(allCards, Seq.empty, Seq.empty, 0, 0, snapOnSuit, snapOnValue)
  }
}