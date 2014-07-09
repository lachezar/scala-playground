package fpinscala.strictnessandlaziness

import org.scalatest.FunSuite

/**
 * Created by lucho on 08/07/14.
 */
class Chapter5Tests extends FunSuite {

  test("toList") {
    assert(Stream(1,2,3).toList == List(1,2,3))
  }

  test("take") {
    assert(Stream(1,2,3).take(2).toList == Stream(1,2).toList)
    assert(Stream(1,2,3).take(5).toList == Stream(1,2,3).toList)
    assert(Stream().take(2).toList == Stream().toList)
    assert(Stream(1).take(0).toList == Stream().toList)
  }

  test("takeWhile") {
    assert(Stream(1,2,3).takeWhile(_ < 3).toList == Stream(1,2).toList)
    assert(Stream(1,2,3).takeWhile(_ < 5).toList == Stream(1,2,3).toList)
    assert((Stream():Stream[Int]).takeWhile(_ < 2).toList == Stream().toList)
    assert(Stream(1).takeWhile(_ < 0).toList == Stream().toList)
  }

  test("exists") {
    println("existing - expected calls = 2")
    assert(Stream(1,2,3).exists(_ == 2))
    println("not exisitng - expected calls = 3")
    assert(!Stream(1,2,3).exists(_ == 5))
  }

  test("forAll") {
    println("forAll - expected calls = 2")
    assert(!Stream(1,2,3).forAll(_ < 2))
    println("forAll - expected calls = 3")
    assert(Stream(1,2,3).forAll(_ < 5))
  }

  test("takeWhile2") {
    println("takeWhile2 - expected calls = 2")
    assert(Stream(1,2,3).takeWhile2(_ < 3).toList == Stream(1,2).toList)
    println("takeWhile2 - expected calls = 3")
    assert(Stream(1,2,3).takeWhile2(_ < 5).toList == Stream(1,2,3).toList)
    println("takeWhile2 - expected calls = 0")
    assert((Stream():Stream[Int]).takeWhile2(_ < 2).toList == Stream().toList)
    println("takeWhile2 - expected calls = 0")
    assert(Stream(1).takeWhile2(_ < 0).toList == Stream().toList)
  }

  test("map") {
    println("map - expected calls = 3")
    assert(Stream(1,2,3).map(_ + 1).toList == Stream(2,3,4).toList)
  }

  test("filter") {
    assert(Stream(1,2,3).filter(_ % 2 == 0).toList == Stream(2).toList)
  }

  test("append") {
    assert(Stream(1,2,3).append(Stream(4,5,6)).toList == Stream(1,2,3,4,5,6).toList)
  }

  test("flatMap") {
    assert(Stream(Stream(1,2), Stream(3,4)).flatMap(x => x).toList == Stream(1,2,3,4).toList)
  }

  test("ones") {
    assert(Stream.ones.take(5).toList == List(1,1,1,1,1))
  }

  test("constant") {
    assert(Stream.constant(3).take(2).toList == List(3,3))
  }

  test("from") {
    assert(Stream.from(1).take(3).toList == List(1,2,3))
  }

  test("fib") {
    assert(Stream.fib().take(7).toList == List(0,1,1,2,3,5,8))
  }

  test("ones2") {
    assert(Stream.ones2.take(5).toList == List(1,1,1,1,1))
  }

  test("constant2") {
    assert(Stream.constant2(3).take(2).toList == List(3,3))
  }

  test("from2") {
    assert(Stream.from2(1).take(3).toList == List(1,2,3))
  }

  test("fib2") {
    assert(Stream.fib2.take(7).toList == List(0,1,1,2,3,5,8))
  }

  test("zip") {
    assert(Stream(1,2,3).zip(Stream(4,5,6)).toList == List((1,4), (2,5), (3,6)))
    assert(Stream(1,2,3,4).zip(Stream(4,5,6)).toList == List((1,4), (2,5), (3,6)))
    println("expect 3 calls")
    assert(Stream(1,2,3).zip(Stream(4,5,6,7)).toList == List((1,4), (2,5), (3,6)))
  }

  test("zipAll") {
    assert(Stream(1,2,3).zipAll(Stream(4,5,6), Nil, Nil).toList == List((1,4), (2,5), (3,6)))
    assert(Stream(1,2,3,4).zipAll(Stream(4,5,6), Nil, Nil).toList == List((1,4), (2,5), (3,6), (4,Nil)))
    println("expect 3 calls")
    assert(Stream(1,2,3).zipAll(Stream(4,5,6,7), Nil, Nil).toList == List((1,4), (2,5), (3,6), (Nil,7)))
  }

  test("startsWith") {
    println(Stream(1,2,3).zipAll(Stream(2,3), None, None).toList)
    assert(Stream(1,2,3).startsWith(Stream(1,2)))
    assert(Stream(1,2).startsWith(Stream(1,2)))
    assert(!Stream(1).startsWith(Stream(1,2)))
    assert(!Stream(1,2,3).startsWith(Stream(1,5)))
    assert(!Stream(1,2,3).startsWith(Stream(2,3)))
  }

  test("drop") {
    assert(Stream(1,2,3).drop(2).toList == Stream(3).toList)
    assert(Stream(1,2,3).drop(5).toList == Stream().toList)
    assert(Stream().drop(2).toList == Stream().toList)
    assert(Stream(1).drop(0).toList == Stream(1).toList)
  }

  test("tails") {
    println(Stream(1,2,3).tails.toList)
    println(Stream(1,2,3).tails.toList.map(_.toList))
    assert(Stream(1,2,3).tails.toList.map(_.toList) == List(List(1,2,3),List(2,3),List(3),List()))
  }

  test("hasSubsequence") {
    assert(Stream.hasSubsequence(Stream(1,2,3,4), Stream(2,3)))
    assert(!Stream.hasSubsequence(Stream(1,2,3,4), Stream(1,5)))
    assert(Stream.hasSubsequence(Stream(1,2,3,4), Stream(1)))
    assert(Stream.hasSubsequence(Stream(1,2,3,4), Stream()))
    assert(!Stream.hasSubsequence(Stream(1,2,3,4), Stream(1,2,3,4,5)))
    assert(Stream.hasSubsequence(Stream(1,2,3,4), Stream(1,2,3,4)))
  }
}
