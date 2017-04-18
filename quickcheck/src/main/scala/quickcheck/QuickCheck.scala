package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(

    const(empty),

    for {
      a <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
  // 1 & 2 fail
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  

  // 2 fails; hint 1
  property("gen2") = forAll { (x: Int, y: Int) =>
    val h: H = empty
    if (x <= y) findMin(insert(y, insert(x, h))) == x
    else findMin(insert(y, insert(x, h))) == y
  }

  // all pass
  property("gen3") = forAll { x: Int =>
    val h = insert(x, empty)
    findMin(h) == x
  }

  // all pass; hint 2
  property("gen4") = forAll { x: Int =>
    val h = insert(x, empty)
    isEmpty(deleteMin(h))
  }

  // 1 & 2 fail; hint 3
  property("gen5") = forAll { h: H =>

    def gen5Helper(h: H, l: List[A]): List[A] = 
      if (isEmpty(h)) l
      else gen5Helper(deleteMin(h), findMin(h) :: l)

    val l = gen5Helper(h, List())
    l == l.sorted.reverse
  }
  
  // all pass; hint 4
  property("gen6") = forAll { (h1: H, h2: H) =>

    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val min1 = findMin(h1) 
      val min2 = findMin(h2)
      val h3 = meld(h1, h2)

      min1 == findMin(h3) || min2 == findMin(h3)
    }    
  }
  
  // 5 fails
  property("gen7") = forAll { (h: H) =>
    meld(h, empty) == h
  }
  
  /* 
  // 1, 2, and 5 fail
  property("gen8") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)

    def gen5Helper(h: H, l: List[A]): List[A] = 
      if (isEmpty(h)) l
      else gen5Helper(deleteMin(h), findMin(h) :: l)

    val l = gen5Helper(h3, List())
    l == l.sorted.reverse
  }
  
  
  // 1, 2, & 5 fail
  property("gen7") = forAll { (h: H, x: Int) =>
    val h1 = insert(x, h)

    def gen5Helper(h: H, l: List[A]): List[A] = 
      if (isEmpty(h)) l
      else gen5Helper(deleteMin(h), findMin(h) :: l)

    val l = gen5Helper(h1, List())
    l == l.sorted.reverse
  }
  

  // all pass
  property("gen8") = {
    val h = meld(empty, empty)
    isEmpty(h) == true
  }
  */

  // 1, 2, 3, & 4 fail
  property("gen9") = forAll { (x: Int, y: Int) =>
    val h1 = insert(y, insert(x, insert(x, empty)))

    def gen5Helper(h: H, l: List[A]): List[A] = 
      if (isEmpty(h)) l
      else gen5Helper(deleteMin(h), findMin(h) :: l)

    val l = gen5Helper(h1, List())
    l == List(x, x, y).sorted.reverse
  }


}
