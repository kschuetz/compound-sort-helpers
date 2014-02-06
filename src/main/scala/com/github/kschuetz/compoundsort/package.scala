package com.github.kschuetz

/**
 * Created by kes on 2/1/14.
 */
package object compoundsort {

  class OrderBy[A](val fn: (A, A) => Boolean)

  implicit def orderBy2Fn[A](orderBy: OrderBy[A]): (A, A) => Boolean = orderBy.fn
  implicit def fn2OrderBy[A](fn: (A, A) => Boolean): OrderBy[A] = new OrderBy[A](fn)


  /**
   * A comparator which always instructs the caller to order the left argument first.
   *
   * Can be used to terminate a chain of compare/compareOptions, as it does not require a tiebreaker function.
   *
   * @param a   The first item to be compared.
   * @param b   The second item to be compared.
   * @tparam A  The type of the item in the collection that is ultimately being sorted.
   * @return    Always returns true.
   */
  def leftFirst[A](a: A, b: A): Boolean =
    true

  /**
   * A comparator which always instructs the caller to order the right argument first.
   *
   * Can be used to terminate a chain of compare/compareOptions, as it does not require a tiebreaker function.
   *
   * @param a   The first item to be compared.
   * @param b   The second item to be compared.
   * @tparam A  The type of the item in the collection that is ultimately being sorted.
   * @return    Always returns false.
   */
  def rightFirst[A](a: A, b: A): Boolean =
    false


  implicit def defaultTiebreaker[A]: OrderBy[A] = leftFirst[A] _


  def ascending[B](implicit ordering: Ordering[B]): (B, B) => Int =
  { (left, right) =>
    ordering.compare(left, right)
  }

  def descending[B](implicit ordering: Ordering[B]): (B, B) => Int =
  { (left, right) =>
    - ordering.compare(left, right)
  }

  def leftIf[B](compareFeatures: (B, B) => Boolean): (B, B) => Int = {
    (x, y) => if(compareFeatures(x, y)) -1 else 1
  }

  def rightIf[B](compareFeatures: (B, B) => Boolean): (B, B) => Int = {
    (x, y) => if(compareFeatures(x, y)) 1 else -1
  }

  /**
   * Returns a comparator function that orders items using a provided comparator that passes the decision to the another comparator in the case of a tie.
   *
   * @param compare  A function to compare the order of two features.  Value returned indicates order:  negative for left first, positive for right first, zero for tie.
   * @param andThenBy  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def orderBy[A](compare: (A, A) => Int)(implicit andThenBy: OrderBy[A]): (A, A) => Boolean = {
    (a, b) => {
      val compared = compare(a, b)
      if (compared == 0) andThenBy.fn(a, b) else (compared < 0)
    }
  }

  /**
   * Returns a comparator function that orders items by first extracting a feature from each item, and then comparing this feature between items.
   * 
   * @param getFeature   A function that extracts a feature from an item.  The value that is returned from this function is what will be compared between two elements in the collection.
   * @param compareFeatures  A function to compare the order of two features.  Value returned indicates order:  negative for left first, positive for right first, zero for tie.
   * @param andThenBy  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @tparam B          The type of the features to be extracted from the collection item.
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def orderByFeature[A, B](getFeature: A => B)(compareFeatures: (B, B) => Int)(implicit andThenBy: OrderBy[A]): (A, A) => Boolean = {
    { (a, b) =>
      val left = getFeature(a)
      val right = getFeature(b)
      val compared = compareFeatures(left, right)
      if(compared == 0) andThenBy.fn(a, b) else (compared < 0)
    }
  }

  /**
   * Returns a comparator function that orders items where the feature extraction fails (i.e. returns None) before those where the extraction succeeds.
   *
   * For those where the extraction succeeds, the items are compared by the function provided in the compareFeatures parameter.
   *
   * @param getFeature   A function that (maybe) extracts a feature from an item.  The value that is returned from this function is what will be compared between two elements in the collection.
   * @param compareFeatures  A function to compare the order of two features.  Value returned indicates order:  negative for left first, positive for right first, zero for tie.
   * @param andThenBy  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @tparam B          The type of the feature that may be extracted from the collection item.
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def orderByFeatureNullsFirst[A, B](getFeature: A => Option[B])(compareFeatures: (B, B) => Int)(implicit andThenBy: OrderBy[A]): (A, A) => Boolean = {
    { (a, b) =>
      (getFeature(a), getFeature(b)) match {
        case (Some(_), None) => false
        case (None, Some(_)) => true
        case (Some(left), Some(right)) => {
          val compared = compareFeatures(left, right)
          if (compared == 0) andThenBy.fn(a, b) else (compared < 0)
        }
        case _ => andThenBy.fn(a, b)
      }
    }
  }

  /**
   * Returns a comparator function that orders items where the feature extraction fails (i.e. returns None) after those where the extraction succeeds.
   *
   * For those where the extraction succeeds, the items are compared by the function provided in the compareFeatures parameter.
   *
   * @param getFeature   A function that (maybe) extracts a feature from an item.  The value that is returned from this function is what will be compared between two elements in the collection.
   * @param compareFeatures  A function to compare the order of two features.  Value returned indicates order:  negative for left first, positive for right first, zero for tie.
   * @param andThenBy  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @tparam B          The type of the feature that may be extracted from the collection item.
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def orderByFeatureNullsLast[A, B](getFeature: A => Option[B])(compareFeatures: (B, B) => Int)(implicit andThenBy: OrderBy[A]): (A, A) => Boolean = {
    { (a, b) =>
      (getFeature(a), getFeature(b)) match {
        case (Some(_), None) => true
        case (None, Some(_)) => false
        case (Some(left), Some(right)) => {
          val compared = compareFeatures(left, right)
          if (compared == 0) andThenBy.fn(a, b) else (compared < 0)
        }
        case _ => andThenBy.fn(a, b)
      }
    }
  }



   /*
  val bar = orderByFeature[String, Int](_.length)(descending){ foo }

  val baz = orderBy(ascending[String]){ bar }

  val quux = orderBy[String](leftIf(_ < _ ))
  */



}
