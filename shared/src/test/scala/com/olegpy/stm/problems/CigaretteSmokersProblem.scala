package com.olegpy.stm.problems

import scala.concurrent.duration._
import scala.util.Random

import cats.effect.IO
import com.olegpy.stm.misc.TQueue
import com.olegpy.stm._
import utest._
import cats.implicits._


object CigaretteSmokersProblem extends NondetIOSuite {
  override def ioTimeout: FiniteDuration = 2.seconds

  val tests = Tests {
    "Cigarette smokers problem" - {
      val attempts = 50
      for {
        table   <- mkTable
        deal    <- new Dealer(table).dealRandom.replicateA(attempts).start
        counter <- TRef.in[IO](0)
        puff    = counter.update(_ + 1).commit[IO]// >> nap
        smoke   <- allIngredients.foldMapM {
          new Smoker(_, table).buildACig(puff).foreverM[Unit].start
        }
        _       <- longNap
        _       <- counter.get.filter(_ == attempts).commit[IO]
        _       <- deal.cancel
        _       <- smoke.cancel
      } yield ()
    }
  }

  sealed trait Ingredient extends Product with Serializable
  case object Tobacco extends Ingredient
  case object Paper extends Ingredient
  case object Matches extends Ingredient

  def allIngredients: List[Ingredient] = List(Tobacco, Paper, Matches)

  class Table(queue: TQueue[Ingredient]) {
    def put(ingredient: Ingredient): STM[Unit] = queue.enqueue(ingredient)
    def takeThings: STM[Set[Ingredient]] = queue.dequeue.replicateA(2).map(_.toSet)
    override def toString: String = s"Table($queue)"
  }

  def mkTable: IO[Table] = TQueue.boundedIn[IO, Ingredient](2).map(new Table(_))

  class Smoker (ingredient: Ingredient, table: Table) {
    def buildACig(puff: IO[Unit]): IO[Unit] =
      table.takeThings.filterNot(_ contains ingredient).commit[IO] >> puff
  }

  class Dealer(table: Table) {
    private val randomIngredients = IO { Random.shuffle(allIngredients).take(2) }
    def dealRandom: IO[Unit] =
      randomIngredients.flatMap { _.traverse_(table.put).commit[IO] }
  }
}
