package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[A]
    m <- oneOf(const(empty), genHeap)
  } yield insert(a, m)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)

  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  //If you insert any two elements into an empty heap,
  //finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("gen2") = forAll { (a: A, b: A) =>
    val m = if (a < b) a else b
    findMin(insert(b, insert(a, empty))) == m
    findMin(insert(a, insert(b, empty))) == m
  }

  //If you insert an element into an empty heap, 
  //then delete the minimum, the resulting heap should be empty
  property("gen3") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  //Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. 
  //(Hint: recursion and helper functions are your friends.)
  property("gen4") = forAll { (h: H) =>
    val l = sortedSeq(h)
    (l, l.tail).zipped.forall(_ <= _)
  }

  def sortedSeq(h: H): List[A] = h match {
    case h if isEmpty(h) => Nil
    case h => findMin(h) :: sortedSeq(deleteMin(h))
  }

  //Finding a minimum of the melding of any two heaps should return a minimum of one or the other
  property("gen5") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = if (m1 < m2) m1 else m2
    findMin(meld(h1, h2)) == m
  }

  property("gen6") = forAll { (a: A, b: A) =>
    isEmpty(deleteMin(deleteMin(insert(a, insert(b, empty)))))
  }

  property("gen7") = forAll { (h: H) =>
    findMin(deleteMin(insert(1, insert(2, empty)))) == 2
  }

  property("gen8") = forAll { (h: H) =>
    findMin(deleteMin(insert(1, insert(2, insert(3, empty))))) == 2
    findMin(deleteMin(insert(3, insert(2, insert(1, empty))))) == 2
    findMin(deleteMin(insert(1, insert(2, insert(2, empty))))) == 2
    findMin(deleteMin(insert(2, insert(1, insert(3, empty))))) == 2
    findMin(deleteMin(insert(2, insert(3, insert(1, empty))))) == 2
  }

}
