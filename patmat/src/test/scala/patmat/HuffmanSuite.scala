package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    assert(times(string2Chars("hello, world")).sorted === List(('h', 1), ('e', 1), ('l', 3), ('o', 2), (',', 1), (' ', 1), ('w', 1), ('r', 1), ('d', 1)).sorted)
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of one leaf list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
  }

  test("combine of Nil list") {
    val leaflist = List()
    assert(combine(leaflist) === List())
  }

  test("createCodeTree") {
    val orderedLeafList = makeOrderedLeafList(times("hello".toList))
    println(orderedLeafList)
    val tree = createCodeTree("hello".toList)
    println(tree)
    assert(1 === 1)
  }

  test("decode test 1") {
    println(decodedSecret)
    assert(1 === 1)
  }

  test("decode test 2") {
    new TestTrees {
      println(decode(t1, List(0, 1)))
      assert(decode(t1, List(0, 1)) === List('a', 'b'))
    }
  }

  test("encode test") {
    new TestTrees {
      println(encode(t1)("ab".toList))
      assert(encode(t1)("ab".toList) === List(0, 1))
    }
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      println(encode(t1)("ab".toList))
      println(decode(t1, List(0, 1)))
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and encode a short text should be identity") {
    new TestTrees {
      println(encode(t1)("baba".toList))
      println(decode(t1, List(0, 1, 0, 1)))
      assert(decode(t1, encode(t1)("baba".toList)) === "baba".toList)
    }
  }

  test("decode and encode a text should be identity") {
    new TestTrees {
      println(encode(t2)("abd".toList))
      assert(decode(t2, encode(t2)("abd".toList)) === "abd".toList)
    }
  }

  test("decode and encode a longer text should be identity") {
    new TestTrees {
      println(encode(t2)("abddba".toList))
      assert(decode(t2, encode(t2)("abdddba".toList)) === "abdddba".toList)
    }
  }

  test("decode and encode a all 'a' text should be identity") {
    new TestTrees {
      println(encode(t2)("aaaa".toList))
      assert(decode(t2, encode(t2)("aaaa".toList)) === "aaaa".toList)
    }
  }

  test("quickEncode - 1") {
    new TestTrees {
      println(quickEncode(t1)("ab".toList))
      println(decode(t1, List(0, 1)))
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("quickEncode - 2") {
    new TestTrees {
      println(quickEncode(t2)("abddba".toList))
      assert(decode(t2, quickEncode(t2)("abddba".toList)) === "abddba".toList)
    }
  }


}
