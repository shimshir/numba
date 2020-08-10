package numba

import java.util.concurrent.Executors

import org.scalatest.Assertion
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.AbstractSeq
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Random, Try}

class NumbaSpec extends AnyFlatSpec with Matchers {
  val numeralSets =
    Seq(
      NumeralSet.Alphabetic,
      NumeralSet.Alphabetic,
      NumeralSet.Alphabetic,
      NumeralSet.Arabic,
      NumeralSet.Arabic,
      NumeralSet.Alphabetic,
      NumeralSet.Alphabetic,
      NumeralSet.Arabic,
      NumeralSet.Arabic
    )

  "Numba" should "be convertible from and to Int" in {
    implicit val ec = ExecutionContext.fromExecutor(
      Executors.newWorkStealingPool(Runtime.getRuntime.availableProcessors())
    )
    val resultsF: Future[Seq[Try[_]]] =
      Future.traverse((1 to Int.MaxValue)) { b10 =>
        Future {
          val fromB10 = Numba.fromBase10(b10, numeralSets)
          val toB10 = Numba.toBase10(fromB10)
          Try(b10 shouldEqual toB10)
        }
      }
    val results = Await.result(resultsF, 7.days)
    val failures = results.filter(_.isFailure)
    failures shouldBe empty
    println(failures)
  }

  "Numba" should "be convertible from and to any Int" in {
    val r = new Random()
    (1 to 100).foreach { _ =>
      val b10 = BigInt(r.nextInt(Int.MaxValue))
      val fromB10 = Numba.fromBase10(b10, numeralSets)
      val toB10 = Numba.toBase10(fromB10)
      //println((b10, fromB10, toB10))
      b10 shouldEqual toB10
    }
  }

  "Numba" should "be random" in {
    Numba.random(numeralSets) should not equal
      Numba.random(numeralSets)
    Numba.random(numeralSets, 42).toString shouldEqual "AH84AR58"
  }
}
