package io.onepintwig.snap

import cats.effect.{IO, IOApp}
import io.onepintwig.snap.api._

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
 * The main runner for the game. Defines game logic, but does not hold game state - which is held entirely within [[Game]]
 */
object Snap extends IOApp.Simple {

  //Determine game parameters, build initial game state, execute game and prompt for recursive replay at the end
  def run(): IO[Unit] = {
    for {
      //Inputs for game
      _ <- IO.println("Number of decks (min 1, max 100)...")
      decks <- intInput(1, 100)
      _ <- IO.println("Match on suit (true/false)...")
      matchOnSuit <- boolInput()
      _ <- IO.println("Match on value (true/false)...")
      matchOnValue <- boolInput()
      //Optional additional param, to give the players time to call snap
      _ <- IO.println("Sleep between goes in seconds (min 0, max 10)...")
      secondsBetweenGoes <- intInput(0, 5)
      //Initialise game state based off defined parameters
      game = Game.init(decks, matchOnSuit, matchOnValue)
      //Play game
      _ <- playRound(game, PlayerOne, secondsBetweenGoes.seconds)
      //Prompt for replay
      _ <- IO.println("Play again? (true/false)...")
      playAgain <- boolInput()
      _ <- if (playAgain) run() else IO.unit
    } yield ()

  }

  //Logic for playing a round of snap
  private def playRound(game: Game, player: Player, timeBetweenGoes: FiniteDuration): IO[Unit] = for {
    //Prompt for player to "turn" card
    _ <- playerGoInput(player)
    //Update game state after card draw
    updatedGameState = game.drawCard(player)
    //Render board to players
    _ <- renderGame(updatedGameState)
    _ <- IO.sleep(timeBetweenGoes)
    //End of turn scoring logic
    updatedGameStateWithScores <- if(updatedGameState.checkSnap) {
      IO.println("Snap detected. Which player called first? (1/2)") *> intInput(1, 2).map {
        case 1 => updatedGameState.updateWinner(PlayerOne)
        case 2 => updatedGameState.updateWinner(PlayerTwo)
      }.flatMap(
        scores => IO.println("Scores updated!") *> renderGame(scores) *> IO.pure(scores)
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
      //Play again with other player
      player match {
        case api.PlayerOne => playRound(updatedGameStateWithScores, PlayerTwo, timeBetweenGoes)
        case api.PlayerTwo => playRound(updatedGameStateWithScores, PlayerOne, timeBetweenGoes)
      }
    }
  } yield ()

  /***** PLAYER INPUTS *****/
  //TODO: Error handling API for better input validation and feedback to players

  private def playerGoInput(player: Player): IO[Unit] =
    IO.println(s"$player go! Press Enter/Return to reveal a card!") *> IO.readLine *> IO.unit

  private def boolInput(): IO[Boolean] = IO.readLine.map(_.toBoolean).handleErrorWith {
    case err: IllegalArgumentException => IO.println(s"Bad input! $err. Try again...") *> boolInput()
    case err => IO.raiseError(err)
  }

  private def intInput(min: Int, max: Int): IO[Int] = IO.readLine.map(_.toInt).flatMap {
    case int if (max < int) || (int < min) =>
      IO.raiseError(new IllegalArgumentException(s"Integer must be min $min and max $max"))
    case int => IO.pure(int)
  }.handleErrorWith {
    case err: IllegalArgumentException => IO.println(s"Bad input! $err. Try again...") *> intInput(min, max)
    case err => IO.raiseError(err)
  }

  /***** GAME RENDERING *****/
  //TODO: Add render method to [[Game]] to make this a bit prettier

  private def renderGame(game: Game): IO[Unit] = {
    IO.println("----------------------------------") *>
    IO.println(
      s"Player1 Score: ${game.player1Score} " +
        s"Player2 Score: ${game.player2Score}"
    ) *> IO.println(
      s"Player1 Stack Top\n${game.player1Stack.lastOption.map(_.render).getOrElse("Empty")} \n" +
        s"Player2 Stack Top\n${game.player2Stack.lastOption.map(_.render).getOrElse("Empty")}"
    ) *> IO.println("----------------------------------")
  }

}
