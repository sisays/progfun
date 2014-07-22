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
  
  test("Update pairs'count"){
    assert(updatePairs('a', List(('a', 3), ('b', 2), ('c', 1))) === List(('a', 4), ('b', 2), ('c', 1)))
    assert(updatePairs('b', List(('a', 3), ('b', 2), ('c', 1))) === List(('a', 3), ('b', 3), ('c', 1)))
    assert(updatePairs('c', List(('a', 3), ('b', 2), ('c', 1))) === List(('a', 3), ('b', 2), ('c', 2)))
    assert(updatePairs('x', List(('a', 3), ('b', 2), ('c', 1))) === List(('a', 3), ('b', 2), ('c', 1), ('x', 1)))
    
  }
  
  test("build up pairs from list of chars and list of pairs"){
    assert(buildUpPairs(List('a', 'b', 'r'), List(('a', 3), ('b', 2))) === List(('a', 4), ('b', 3), ('r', 1)))
  }
  
  test("times on a::b::a"){
    assert(times(List('a', 'b', 'a'))===List(('a', 2), ('b', 1)))
  }
  
  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }
  
  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("combine of a Singleton or Nil") {
    assert(combine(List(Leaf('e', 1))) === List(Leaf('e',1)))
    assert(combine(Nil) === Nil)
  }
  
  test("createCodeTree(someText)' gives an optimal encoding, the number of bits when encoding 'someText' is minimal"){
    assert(createCodeTree(List('a','b')) === Fork(Leaf('a',1), Leaf('b',1), List('a','b'), 2))
  }
  
  test("display secret"){
    println("decoded secret: " + decodedSecret)
  }
  
  test("encode huffmanestcool"){
    val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
    assert(encodedSecret === secret)
  }

  test("quick encode huffmanestcool"){
    val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
    assert(quickEncodedSecret === secret)
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("decode and quick encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
