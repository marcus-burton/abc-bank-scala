package com.abc.memory

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.tagobjects.Slow

class TransactionalTest extends WordSpec with Matchers {

  "Transactional.apply" should {
    "construct a new Transactional object" when {
      "called" in {
        val transactionA, transactionB: Transactional[Int] = Transactional(42)
        transactionA shouldNot be theSameInstanceAs transactionB
      }
    }
  }

  "peek" should {
    "return the wrapped value" when {
      "it has never been mutated" in { Transactional(42).peek shouldBe 42 }
    }

    "not mutate the underlying value" when {
      "called repeatedly" in {
        val state = Transactional(42)
        state.peek shouldBe state.peek
      }
    }
  }

  "commit" should {
    "update state and return a result per the step function" when {
      "there is no contention" in {
        val state = Transactional("")
        "hello".map(c => state.commit(s => (c +: s, c.toUpper))) shouldBe "HELLO"
        state.peek shouldBe "olleh"
      }

      // Typically takes about 30 seconds on a 2013 Macbook Pro.
      "there is contention" taggedAs (Slow) in {
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.{Await, Future}
        import scala.concurrent.duration.Duration
        val rand = new scala.util.Random(seed = 42)
        val state = Transactional(0)
        val ints = 1 to 1000
        val futures = ints.map(i =>
          Future(state.commit { n =>
            Thread.sleep(rand.nextInt(100))
            (n + i, i * 2)
          }))
        val results = Await.result(Future.sequence(futures), Duration.Inf)
        results shouldBe ints.map(_ * 2)
        state.peek shouldBe ints.sum
      }
    }
  }
}
