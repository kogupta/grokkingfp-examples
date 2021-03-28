import cats.effect.{IO, Ref}
import cats.implicits._

import scala.concurrent.duration._

object ch10_CastingDieConcurrently extends App {
  import ch08_CastingDieImpure.NoFailures.castTheDieImpure

  def castTheDie(): IO[Int] = IO.delay(castTheDieImpure())

  // Practicing refs and concurrent IOs:
  // 1. wait 1 second, then cast two dies concurrently, wait for both of them and return their sum
  check.executedIO(for {
    _      <- IO.sleep(1.second) // introduce 1.second
    result <- List(castTheDie(), castTheDie()).parSequence
  } yield result)

  // 2. cast two dies concurrently, store each result in an atomic reference that holds a List, and return the final list as a result
  check.executedIO(for {
    storedCasts <- Ref.of[IO, List[Int]](List.empty)
    singleCast  = castTheDie().flatMap(result => storedCasts.update(_.appended(result)))
    _           <- List(singleCast, singleCast).parSequence
    casts       <- storedCasts.get
  } yield casts)

  // 3. cast three dies concurrently, store each result in an atomic reference that holds a List, and return its value as a result
  check.executedIO(for {
    storedCasts <- Ref.of[IO, List[Int]](List.empty)
    singleCast  = castTheDie().flatMap(result => storedCasts.update(_.appended(result)))
    _           <- List.fill(3)(singleCast).parSequence // introduce List.fill
    casts       <- storedCasts.get
  } yield casts)

  // 4. cast one hundred dies concurrently, store the total number of sixes in an atomic ref, and return its value as a result
  check.executedIO(for {
    storedCasts <- Ref.of[IO, Int](0)
    singleCast  = castTheDie().flatMap(result => if (result == 6) storedCasts.update(_ + 1) else IO.unit)
    _           <- List.fill(100)(singleCast).parSequence
    casts       <- storedCasts.get
  } yield casts)

  // 5. cast one hundred dies concurrently, wait 1 second before each of them, and return their sum (without using an atomic ref)
  check.executedIO(List.fill(100)(IO.sleep(1.second).flatMap(_ => castTheDie())).parSequence.map(_.sum))
}
