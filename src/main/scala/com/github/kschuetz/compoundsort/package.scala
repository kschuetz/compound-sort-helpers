package com.github.kschuetz

/**
 * Created by kes on 2/1/14.
 */
package object compoundsort {

  /**
   * Returns a comparator function that orders items by first extracting the sort criteria, and the comparing the extracted criteria.
   * 
   * @param extractor   A function that extracts criteria from an item.  This criteria is what will be compared between two elements in the collection.
   * @param comparator  A function to compare the order of two Bs.  This function should return true if the left argument should appear earlier in the sort order than the right.
   * @param tiebreaker  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @tparam B          The type of the sort criteria that may be extracted from the collection item
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def compare[A, B](extractor: A => B)(comparator: (B, B) => Boolean)(tiebreaker: (A, A) => Boolean): (A, A) => Boolean = {
    { (a, b) =>
      val left = extractor(a)
      val right = extractor(b)
      if(left != right) comparator(left, right)
      else tiebreaker(a, b)
    }
  }


  /**
   * Returns a comparator function that orders items where the criteria extraction fails (i.e. returns None) before those where the extraction succeeds.
   *
   * For those where the extraction succeeds, the items are compared by the function provided in the comparator parameter.
   *
   * @param extractor   A function that (maybe) extracts criteria from an item.  This criteria is what will be compared between two elements in the collection.
   * @param comparator  A function to compare the order of two Bs.  This function should return true if the left argument should appear earlier in the sort order than the right.
   * @param tiebreaker  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @tparam B          The type of the sort criteria that may be extracted from the collection item
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def compareOptionNullsFirst[A, B](extractor: A => Option[B])(comparator: (B, B) => Boolean)(tiebreaker: (A, A) => Boolean): (A, A) => Boolean = {
    { (a, b) =>
      (extractor(a), extractor(b)) match {
        case (Some(_), None) => false
        case (None, Some(_)) => true
        case (Some(left), Some(right)) if left != right => comparator(left, right)
        case _ => tiebreaker(a, b)
      }
    }
  }
  
  /**
   * Returns a comparator function that orders items where the criteria extraction fails (i.e. returns None) before those where the extraction succeeds.
   *
   * For those where the extraction succeeds, the items are compared by the function provided in the comparator parameter.
   *
   * @param extractor   A function that (maybe) extracts criteria from an item.  This criteria is what will be compared between two elements in the collection.
   * @param comparator  A function to compare the order of two Bs.  This function should return true if the left argument should appear earlier in the sort order than the right.
   * @param tiebreaker  The next comparison to try if the comparison results in a tie.
   * @tparam A          The type of the item in the collection that is ultimately being sorted.
   * @tparam B          The type of the sort criteria that may be extracted from the collection item
   * @return            A function that can be used as an argument to the sortWith method of a collection, or as the comparator in other
   *                    functions in the compoundsort library.
   */
  def compareOptionNullsLast[A, B](extractor: A => Option[B])(comparator: (B, B) => Boolean)(tiebreaker: (A, A) => Boolean): (A, A) => Boolean = {
    { (a, b) =>
      (extractor(a), extractor(b)) match {
        case (Some(_), None) => true
        case (None, Some(_)) => false
        case (Some(left), Some(right)) if left != right => comparator(left, right)
        case _ => tiebreaker(a, b)
      }
    }
  }

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

}
