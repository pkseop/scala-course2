package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for{
    a <- arbitrary[A]
    h <- frequency[H]((1, const[H](empty)), (9, genHeap))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (a: A, b: A) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == Math.min(a, b)
  }

  property("hint2") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("hint3") = forAll { (h: H) =>
    def loop(h: H, acc: List[A]): List[A] = {
      if(isEmpty(h))
        acc
      else {
        val v = findMin(h)
        val next = deleteMin(h)
        if (isEmpty(next))
          acc :+ v
        else {
          loop(next, acc :+ v)
        }
      }
    }
    val res = loop(h, List.empty[A])
    (res, res.tail).zipped.forall(_ <= _)
  }

  property("hint4") = forAll { (a: H, b: H) =>
    val minA = findMin(a)
    val minB = findMin(b)
    findMin(meld(a, b)) == Math.min(minA, minB)
  }

  property("etc1") = forAll { (h: H) =>
    val v = findMin(h)
    v == findMin(insert(v, deleteMin(h)))
  }

  property("etc2") = forAll { (h: H) =>
    val v = findMin(h)
    val d = deleteMin(h)
    v == findMin(insert(v, d))
  }

  property("etc3") = forAll { (h1: H, h2: H) =>
    def loop(h: H, acc: List[Int]): List[Int] = {
      if (isEmpty(h))
        acc
      else
        loop(deleteMin(h), acc :+ findMin(h))
    }
    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val list1 = loop(meld1, Nil)
    val list2 = loop(meld2, Nil)
    list1 == list2
  }
}
