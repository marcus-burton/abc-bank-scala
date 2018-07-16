package com.abc.memory

import java.util.concurrent.atomic.AtomicReference

object Transactional {
  def apply[T](value: T): Transactional[T] =
    new Transactional[T](new AtomicReference(value))
}

final class Transactional[T] private (ref: AtomicReference[T]) {

  def peek(): T = ref.get

  @scala.annotation.tailrec
  def commit[R](step: (T) => (T, R)): R = {
    val before = ref.get
    val (after, result) = step(before)
    if (ref.compareAndSet(before, after)) result
    // $COVERAGE-OFF$
    else commit(step)
    // $COVERAGE-ON$
  }
}
