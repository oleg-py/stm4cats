# stm4cats
![Sonatype Nexus (Snapshots)](https://img.shields.io/nexus/s/https/oss.sonatype.org/com.olegpy/stm4cats_2.12.svg)

An implementation of STM for any cats-effect compatible effect type.

Experiment goals:
* Radically different approach, lock-free instead of a global lock, and ThreadLocals instead of Reader-like structure.
  * It's still to be figured out if it's faster than conventional implementation with a global lock, or not.
* Keep everything as minimal as humanly possible. Right now there are only about 300 lines of code.
  * However, you need to import `cats.implicits._` for all syntax to work
* Scala.JS support is a must.

## Add it
```scala
resolvers += Resolvers.sonatypeRepo("snapshots")
// Use %%% for Scala.JS
libraryDependencies += "com.olegpy" %% "stm4cats" % "0.1.0-SNAPSHOT"
```

### Try it
```scala
import cats.implicits._
import cats.effect.IO
import com.olegpy.stm._
import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

implicit val cs = IO.contextShift(global)
implicit val timer = IO.timer(global)


def transfer(fromR: TRef[Int], toR: TRef[Int], amt: Int): STM[Unit] =
  for {
    from <- fromR.get
    if from >= amt // Or STM.check(from >= amt)
    _ <- fromR.modify(_ - amt)
    _ <- toR.modify(_ + amt)
  } yield ()
  
def freeMoney(toR: TRef[Int]): IO[Unit] = STM.atomically[IO] {
  toR.modify(_ + 10)
}

val io = for {
  fromR <- TRef(0).commit[IO]
  // Or shorter syntax:
  toR   <- TRef.in[IO](0)
  amt   =  100

  f1    <- transfer(fromR, toR, amt).commit[IO].start
  f2    <- (freeMoney(fromR) >> IO.sleep(1.second)).foreverM.start
  // In 10 seconds, the transfer succeeds
  _     <- f1.join
  _     <- f2.cancel
  res   <- toR.get.commit[IO]
  _     <- IO(assert(res == amt))
} yield ()

io.unsafeRunSync() // Well, not on JS
```
