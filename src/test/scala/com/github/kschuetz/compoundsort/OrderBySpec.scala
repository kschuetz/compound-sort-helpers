
import scala.collection.immutable._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._


object OrderBySpec extends Properties("OrderBy") {

  import java.util.Date
  import com.github.kschuetz.compoundsort._

  case class Thing(
    foo: Int,
    bar: Option[Int],
    baz: Boolean,
    qux: String,
    quux: Option[String],
    waldo: (Int, Int),
    fred: Seq[Int]
  )

  lazy val genString: Gen[String] = Gen.choose(1, 8) flatMap { sz =>
    Gen.listOfN(sz, Gen.choose('a', 'z')).map { _.mkString }
  }

  implicit val arbString: Arbitrary[String] = Arbitrary(genString)

  lazy val genOptionString: Gen[Option[String]] = Gen.choose(0, 8) flatMap { sz =>
    Gen.listOfN(sz, Gen.choose('a', 'z')).map {
      case Nil => None
      case xs => Some(xs.mkString)
    }
  }

  implicit val arbOptionString: Arbitrary[Option[String]] = Arbitrary(genOptionString)

  lazy val genOptionInt: Gen[Option[Int]] = for {
    n <- arbitrary[Int]
  } yield Some(n).filter(_ % 3 != 0)

  implicit val arbOptionInt: Arbitrary[Option[Int]] = Arbitrary(genOptionInt)


  lazy val generateThing: Gen[Thing] = for {
    foo <- arbitrary[Int]
    bar <- arbitrary[Option[Int]]
    baz <- arbitrary[Boolean]
    qux <- arbitrary[String]
    quux <- arbitrary[Option[String]]
    waldo <- arbitrary[(Int, Int)]
    fred <- arbitrary[Seq[Int]]
  } yield Thing(
      foo, bar, baz, qux, quux, waldo, fred
    )

  implicit lazy val arbThing: Arbitrary[Thing] = Arbitrary(generateThing)

  lazy val generateThings: Gen[Seq[Thing]] = {
    Gen.choose(5, 25) flatMap { sz => Gen.listOfN(sz, generateThing) }
  }

  implicit lazy val arbThings: Arbitrary[Seq[Thing]] = Arbitrary(generateThings)

  val sortOrder1 =
    orderByFeature[Thing, Int](_.foo)(ascending){
      orderByFeatureNullsFirst[Thing, Int](_.bar)(descending)
    }

  val sortOrder2 = orderByFeature[Thing, Boolean](_.baz)(ascending) {
    orderByFeature[Thing, Int](_.waldo._1)(descending) {
      orderByFeature[Thing, Int](_.waldo._2)(ascending)
    }
  }

  def isAscending[A](coll: Seq[A])(implicit ord: Ordering[A]) = {
    coll.isEmpty || (coll, coll.tail).zipped.forall(ord.compare(_,_) <= 0)
  }

  def isDescending[A](coll: Seq[A])(implicit ord: Ordering[A]) = {
    coll.isEmpty || (coll, coll.tail).zipped.forall(ord.compare(_,_) >= 0)
  }

  def isAscendingNullsFirst[A](coll: Seq[Option[A]])(implicit ord: Ordering[A]) = {
    val nonNulls = coll.dropWhile(_.isEmpty)
    nonNulls.forall(_.nonEmpty) && isAscending(nonNulls)
  }

  def isAscendingNullsLast[A](coll: Seq[Option[A]])(implicit ord: Ordering[A]) = {
    val (nonNulls, nulls) = coll.span(_.nonEmpty)
    nulls.forall(_.isEmpty) && isAscending(nonNulls)
  }

  def isDescendingNullsFirst[A](coll: Seq[Option[A]])(implicit ord: Ordering[A]) = {
    val nonNulls = coll.dropWhile(_.isEmpty)
    nonNulls.forall(_.nonEmpty) && isDescending(nonNulls)
  }

  def isDescendingNullsLast[A](coll: Seq[Option[A]])(implicit ord: Ordering[A]) = {
    val (nonNulls, nulls) = coll.span(_.nonEmpty)
    nulls.forall(_.isEmpty) && isDescending(nonNulls)
  }

  def groups[A, K](coll: Seq[A])(f: (A) => K): Seq[Seq[A]] =
    coll.groupBy(f).toList.map(_._2)

  def isIdempotent[A](coll: Seq[A])(order: (A, A) => Boolean): Boolean = {
    val sort1 = coll.sortWith(order)
    val sort2 = sort1.sortWith(order)
    sort2.corresponds(sort1)(_ == _)
  }


  property("sortOrder1 correct") = forAll { things: Seq[Thing] =>
    val sorted = things.sortWith(sortOrder1)
    isAscending(sorted.map(_.foo)) && {
      groups(sorted)(_.foo).forall { xs =>
        isDescendingNullsFirst(xs.map(_.bar))
      }
    }
  }

  property("sortOrder2 correct") = forAll { things: Seq[Thing] =>
    val sorted = things.sortWith(sortOrder2)
    isAscending(sorted.map(_.baz)) && {
      groups(sorted)(_.baz).forall { xs =>
        isDescending(xs.map(_.waldo._1)) && {
          groups(sorted)(_.waldo._2).forall { xs =>
            isAscending(xs.map(_.waldo._2))
          }
        }
      }
    }
  }

  /*
    In order for a sort to be idempotent, all fields must be specified

  property("sortOrder1 idempotent") = forAll { things: Seq[Thing] =>
    isIdempotent(things)(sortOrder1)
  }

  property("sortOrder2 idempotent") = forAll { things: Seq[Thing] =>
    isIdempotent(things)(sortOrder2)
  }

   */

}