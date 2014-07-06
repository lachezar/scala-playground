/**
 * Created by lucho on 06/07/14.
 */

package fpinscala.datastructures

import org.scalatest._

class Chapter3Tests extends FlatSpec with Matchers {

  "A list" should "have a sum" in {
    List.sum(List(1,2,3)) should be (6)
  }

  "A list of doubles" should "have a product" in {
    List.product(List(1.0,2.0,3.0)) should be (6.0)
  }

  "A list" should "be constructable" in {
    Cons(1, Cons(2, Cons(3, Nil))) should be (List(1,2,3))
  }

  "List.tail" should "remove first element" in {
    List.tail(List(1,2,3)) should be (List(2,3))
    List.tail(List()) should be (List())
  }

  "List.drop" should "remove n first elements" in {
    List.drop(List(1,2,3), 2) should be (List(3))
    List.drop(List(1,2,3), 4) should be (List())
    List.drop(List(1,2,3), 0) should be (List(1,2,3))
    List.drop(List(), 5) should be (List())
  }

  "List.dropWhile" should "remove elements where f(e) is false" in {
    var dwc = List.dropWhile(List(1,2,3))(_)
    dwc(x => x % 2 == 0) should be (List(2))
    dwc(x => x < 2) should be (List(1))
    dwc(x => x > 5) should be (List())
    dwc(x => true) should be (List(1,2,3))
    dwc(x => false) should be (List())
    List.dropWhile(List())(x => true) should be (List())
  }

  "List.setHead" should "swap the first element of the list with new value" in {
    List.setHead(List(1,2,3), 5) should be (List(5,2,3))
    List.setHead(List(), 5) should be (List())
    List.setHead(List(1), 3) should be (List(3))
  }

  "List.append" should "concatinate 2 lists" in {
    List.append(List(), List()) should be (List())
    List.append(List(1,2), List(3)) should be (List(1,2,3))
  }

  "List.init" should "return the same list without the last element" in {
    List.init(List()) should be (List())
    List.init(List(1)) should be (List())
    List.init(List(1,2,3)) should be (List(1,2))
  }

  "A list of doubles" should "have a product using foldRight" in {
    List.product2(List(1.0,2.0,3.0)) should be (6.0)
  }

  "A list of integers" should "have a sum using foldRight" in {
    List.sum2(List(1,2,3)) should be (6)
  }

  "A list of doubles" should "have a sum using foldRight" in {
    List.sum3(List(1.0,2.0,3.0)) should be (6.0)
  }

  "foldRight" should "do something crazy" in {
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
  }

  "List.length" should "give the size of the list" in {
    List.length(List(1,2,3)) should be (3)
    List.length(Nil:List[Double]) should be (0)
  }

  "A list of doubles" should "have a product using foldLeft" in {
    List.productfl(List(1.0,2.0,3.0)) should be (6.0)
  }

  "A list of integers" should "have a sum using foldLeft" in {
    List.sumfl(List(1,2,3)) should be (6)
  }

  "List.lengthfl" should "give the size of the list" in {
    List.lengthfl(List(1,2,3)) should be (3)
    List.lengthfl(Nil:List[Double]) should be (0)
  }

  "List.reverse" should "reverse the list" in {
    List.reverse(List(1,2,3)) should be (List(3,2,1))
    List.reverse(List(1)) should be (List(1))
    List.reverse(List()) should be (List())
  }

  "List.append2" should "append 2 lists" in {
    List.append2(List(1,2), List(3,4)) should be (List(1,2,3,4))
  }

  "List.append3" should "append 2 lists" in {
    List.append3(List(1,2), List(3,4)) should be (List(1,2,3,4))
  }

  "List.concat" should "concat list of lists in a single list" in {
    List.concat(List(List(1,2), List(3,4), List(5,6), List(7,8,9))) should be (List(1,2,3,4,5,6,7,8,9))
    List.concat(List(List(1,2))) should be (List(1,2))
    List.concat(List()) should be (List())
  }

  "List.plus1" should "increment each element" in {
    List.plus1(List(1,2,3)) should be (List(2,3,4))
  }

  "List.stringify" should "make each element a string" in {
    List.stringify(List(1.0,2.0,3.0)) should be (List("1.0","2.0","3.0"))
  }

  "List.filter" should "remove elements not matching a condition" in {
    List.filter(List(1,2,3))(_ % 2 == 0) should be (List(2))
  }

  "List.flatMap" should "be like a map that returns flatten list of lists" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "List.filter2" should "remove elements not matching a condition" in {
    List.filter2(List(1,2,3))(_ % 2 == 0) should be (List(2))
  }

  "List.zip" should "combine pair of elements" in {
    List.zip(List(1,2,3), List(4,5,6))(_+_) should be (List(5,7,9))
    List.zip(List(1,2), List(4,5,6))(_+_) should be (List(5,7))
    List.zip(List(1,2,3), List(4))(_+_) should be (List(5))
  }

  "List.hasSubsequence" should "find subsequence in a list" in {
    List.hasSubsequence(List(1,2,3), List(1,2,3)) should be(true)
    List.hasSubsequence(List(1,2,3), List(2,3)) should be(true)
    List.hasSubsequence(List(1,2,3), List(1,2)) should be(true)
    List.hasSubsequence(List(1,2,3), List(1,2,3,4)) should be(false)
    List.hasSubsequence(List(1,2,3), List(1,5,3)) should be(false)
    List.hasSubsequence(List(1,2,3), List(5)) should be(false)
    List.hasSubsequence(List(1,2,3), List(3)) should be(true)
    List.hasSubsequence(List(1,2,3), Nil) should be(true)
    List.hasSubsequence(Nil, Nil) should be(true)
    List.hasSubsequence(Nil, List(1,2,3)) should be(false)
  }

  "Tree.size" should "give the amount of nodes" in {
    Tree.size(Branch(Leaf(5), Branch(Leaf(6), Leaf(7)))) should be (5)
    Tree.size(Leaf(5)) should be (1)
  }

  "Tree.maximum" should "give the maximum value of all leafs" in {
    Tree.maximum(Branch(Leaf(5), Branch(Leaf(7), Leaf(6)))) should be (7)
    Tree.maximum(Leaf(5)) should be (5)
  }

  "Tree.depth" should "give the maximum depth of the tree" in {
    Tree.depth(Branch(Leaf(5), Branch(Branch(Leaf(7), Branch(Leaf(9), Leaf(8))), Leaf(6)))) should be (5)
    Tree.depth(Leaf(5)) should be (1)
  }

  "Tree.map" should "create a new tree and apply a function on the leafs" in {
    Tree.map(Branch(Leaf(5), Branch(Branch(Leaf(7), Branch(Leaf(9), Leaf(8))), Leaf(6))))(_ * 2) should be (
      Branch(Leaf(10), Branch(Branch(Leaf(14), Branch(Leaf(18), Leaf(16))), Leaf(12))))
    Tree.map(Leaf(5))(_ * 2) should be (Leaf(10))
  }
}
