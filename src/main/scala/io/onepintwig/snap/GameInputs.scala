package io.onepintwig.snap

import cats.effect.IO
import io.onepintwig.snap.api.Player

/**
 * Controls all player input into the game
 */
object GameInputs {

  def playerGoInput(player: Player): IO[Unit] =
    IO.println(s"$player go! Press Enter/Return to reveal a card!") *> IO.readLine *> IO.unit

  def boolInput(prompt: String): IO[Boolean] =
    IO.println(prompt + " (true/false)") *> IO.readLine.map(_.toBoolean).handleErrorWith {
      case err: IllegalArgumentException => IO.println(s"Bad input! $err. Try again...") *> boolInput(prompt)
      case err => IO.raiseError(err)
    }

  def intInput(prompt: String, min: Int, max: Int): IO[Int] =
    IO.println(prompt + s" (min: $min, max: $max)") *> IO.readLine.map(_.toInt).flatMap {
      case int if (max < int) || (int < min) =>
        IO.raiseError(new IllegalArgumentException(s"Integer must be min $min and max $max"))
      case int => IO.pure(int)
    }.handleErrorWith {
      case err: IllegalArgumentException => IO.println(s"Bad input! $err. Try again...") *> intInput(prompt, min, max)
      case err => IO.raiseError(err)
    }

}
