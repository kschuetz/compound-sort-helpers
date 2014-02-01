package com.kschuetz

/**
 * Created by kes on 2/1/14.
 */
package object compoundsort {

  def compareOptionNullsLast[A, B](extractor: A => Option[B])(comparator: (B, B) => Boolean)(next: (A, A) => Boolean): (A, A) => Boolean = {
    { (a, b) =>
      (extractor(a), extractor(b)) match {
        case (Some(_), None) => true
        case (None, Some(_)) => false
        case (Some(left), Some(right)) if left != right => comparator(left, right)
        case _ => next(a, b)
      }
    }
  }

  def compareOptionNullsFirst[A, B](extractor: A => Option[B])(comparator: (B, B) => Boolean)(next: (A, A) => Boolean): (A, A) => Boolean = {
    { (a, b) =>
      (extractor(a), extractor(b)) match {
        case (Some(_), None) => false
        case (None, Some(_)) => true
        case (Some(left), Some(right)) if left != right => comparator(left, right)
        case _ => next(a, b)
      }
    }
  }

  def compare[A, B](extractor: A => B)(comparator: (B, B) => Boolean)(next: (A, A) => Boolean): (A, A) => Boolean = {
    { (a, b) =>
      val left = extractor(a)
      val right = extractor(b)
      if(left != right) comparator(left, right)
      else next(a, b)
    }
  }

  def leftFirst[A](a: A, b: A): Boolean =
    true

  def rightFirst[A](a: A, b: A): Boolean =
    false

}
