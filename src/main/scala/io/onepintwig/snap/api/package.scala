package io.onepintwig.snap

//API For Cards and Players
package object api {

  sealed trait CardSuit
  private [api] case object Hearts extends CardSuit
  private [api] case object Spades extends CardSuit
  private [api] case object Diamonds extends CardSuit
  private [api] case object Clubs extends CardSuit

  private [api] case class Card(suit: CardSuit, value: Int){
    /** Pretty print a card */
    def render: String = {
      val valuePrint = value match {
        case 11 => "J"
        case 12 => "Q"
        case 13 => "K"
        case 14 => "A"
        case num => num.toString
      }
      val suitPrint = suit match {
        case Hearts => "H"
        case Spades => "S"
        case Diamonds => "D"
        case Clubs => "C"
      }
      "┌─────┐\n" +
      s"|$valuePrint    |\n" +
      "|  *  |\n" +
      s"|    $suitPrint|\n" +
      "└─────┘\n"
    }

    /**
     *
     * @param card The card to compare with this for equality
     * @param snapValue If true, will say snap is true if the provide card value matches
     * @param snapSuit If true, will say snap is true if the provided card suit matches
     * @return
     */
    def snapCheck(card: Card, snapValue: Boolean, snapSuit: Boolean): Boolean =
      (snapValue && (this.value == card.value)) || (snapSuit && (this.suit == card.suit))
  }

  sealed trait Player
  case object PlayerOne extends Player
  case object PlayerTwo extends Player

}
