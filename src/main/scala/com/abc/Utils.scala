package com.abc

object Utils {
  /**
   * Like `map`, but maps 2 elements at a time.
   * If a list with a single element is passed, no action is done.
   */
  def map2[A](f: (A, A) => A, list: List[A]): List[A] = list match {
    case Nil => Nil
    case _ :: Nil => list
    case (x::y::xs) => f(x, y) :: map2(f, y::xs)
  }
}
