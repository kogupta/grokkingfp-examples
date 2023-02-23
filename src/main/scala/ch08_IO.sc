import cats.effect.IO
import cats.implicits._
import cats.effect.unsafe.implicits.global

import ch08_CastingDieImpure.WithFailures.castTheDieImpure
import ch08_CastingDieImpure.drawAPointCard
import ch08_SchedulingMeetings.Version2

val zero = IO.pure(0)

// Cast the die, and if it fails to produce a result, return 0.
val one = IO.delay(castTheDieImpure())
  .orElse(zero)

// Draw a card, and if it fails, cast the die.
val two = IO.delay(drawAPointCard())
  .orElse(IO.delay(castTheDieImpure()))

//Cast the die, and if it fails—retry once. If it fails again, return 0.
val three = IO.delay(castTheDieImpure())
  .orElse(IO.delay(castTheDieImpure()))
  .orElse(zero)

//Cast the die, and draw a card, using a fallback of 0 for each of them. Return the sum of both.
val four: IO[Int] = for {
  die <- one
  card <- IO.delay(drawAPointCard()).orElse(zero)
} yield die + card

//Draw a card, and cast the die twice. Return the sum of all three or 0 if any of them fails.
val cardDieDie = for {
  card <- IO.delay(drawAPointCard())
  die <- IO.delay(castTheDieImpure())
  die2 <- IO.delay(castTheDieImpure())
} yield card + die + die2
val five: IO[Int] = cardDieDie.orElse(zero)


def schedulingProgram(getName: IO[String],
                      showMeeting: Option[MeetingTime] => IO[Unit])
: IO[Unit] = {
  for {
    a <- getName
    b <- getName
    possibleMeeting <- Version2.schedule(a, b, 2)
    _ <- showMeeting(possibleMeeting)
  } yield ()
}

import ch08_SchedulingMeetings.consolePrint
import ch08_SchedulingMeetings.consoleGet

schedulingProgram(
  IO.delay(consoleGet()),
  meeting => IO.delay(consolePrint(meeting.toString))
).unsafeRunSync()

