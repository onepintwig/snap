package io.onepintwig.snap

import cats.effect.IO
import io.onepintwig.snap.api.Game

/**
 * Handles rendering of the game board
 */
object GameRenderer {

  //TODO: Add render method to [[Game]] to make this a bit prettier
  //TODO: Async Render
  def renderGame(game: Game): IO[Unit] = for {
    _ <- IO.println("----------------------------------")
    _ <-  IO.println(
      s"Player1 Score: ${game.player1Score} " +
        s"Player2 Score: ${game.player2Score}"

    )
    _ <- IO.println(
      s"Player1 Stack Top\n${game.player1Stack.lastOption.map(_.render).getOrElse("Empty")} \n" +
        s"Player2 Stack Top\n${game.player2Stack.lastOption.map(_.render).getOrElse("Empty")}"
    )
    _ <- IO.println("----------------------------------")
  } yield ()
}
