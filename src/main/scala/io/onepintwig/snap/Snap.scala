package io.onepintwig.snap

import cats.effect.{IO, IOApp}
import io.onepintwig.snap.api._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
 * The main runner for the game.
 * Defines game logic, but does not hold game state - which is held entirely within [[Game]]
 */
object Snap extends IOApp.Simple {

  //Determine game parameters, build initial game state, execute game and prompt for recursive replay at the end
  def run(): IO[Unit] = {
    for {
      //Inputs for game
      decks <- GameInputs.intInput("Number of decks", 1, 100)
      matchOnSuit <- GameInputs.boolInput("Match on suit")
      matchOnValue <-GameInputs. boolInput("Match on value")
      //Optional additional param, to give the players time to call snap
      secondsBetweenGoes <- GameInputs.intInput("Sleep between goes in seconds", 0, 5)
      //Initialise game state based off defined parameters
      game = Game.init(decks, matchOnSuit, matchOnValue)
      //Play game
      //TODO: Random player for first turn
      _ <- playRound(game, PlayerOne, secondsBetweenGoes.seconds)
      //Prompt for replay
      playAgain <- GameInputs.boolInput("Play again?")
      _ <- if (playAgain) run() else IO.println("Thanks for playing!")
    } yield ()

  }

  //Logic for playing a round of snap
  //Recursively plays a round until all cards are played
  private def playRound(game: Game, player: Player, timeBetweenGoes: FiniteDuration): IO[Unit] = for {
    //Prompt for player to "turn" card
    _ <- GameInputs.playerGoInput(player)
    //Update game state after card draw
    updatedGameState = game.drawCard(player)
    //Render updated board to players
    _ <- GameRenderer.renderGame(updatedGameState)
    _ <- IO.sleep(timeBetweenGoes)
    //End of turn scoring logic
    updatedGameStateWithScores <- if(updatedGameState.checkSnap) {
      //TODO: Button press for snap? Detect which player hits first - punish mishit
      GameInputs.intInput("Snap detected. Which player called first?", 1, 2).map {
        case 1 => updatedGameState.updateWinner(PlayerOne)
        case 2 => updatedGameState.updateWinner(PlayerTwo)
      }.flatMap(
        game =>
          //Render game board so players can see scores
          IO.println("Scores updated!") *> GameRenderer.renderGame(game) *> IO.pure(game)
      )
    } else IO.pure(updatedGameState)
    //End game if remaining cards are empty, else play again for the other player
    _ <- if (updatedGameStateWithScores.remainingCards.isEmpty) {
      //Game over
      if (updatedGameStateWithScores.player1Score == updatedGameStateWithScores.player2Score)
        IO.println("Draw!") else {
        val winner = if (updatedGameStateWithScores.player1Score > updatedGameStateWithScores.player2Score)
          PlayerOne
        else
          PlayerTwo
        IO.println(s"$winner wins! Congratulations!!")
      }
  } else {
      //Other player draws a card
      val nextPlayersTurn = player match {
        case api.PlayerOne => PlayerTwo
        case api.PlayerTwo => PlayerOne
      }
      playRound(updatedGameStateWithScores, nextPlayersTurn, timeBetweenGoes)
    }
  } yield ()

}
