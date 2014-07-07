package fpinscala.errorhandling

import org.scalatest.{FunSuite}

/**
 * Created by lucho on 07/07/14.
 */
class Chapter4Tests extends FunSuite {

  test("variance") {
    assert(Exercises.variance(Seq(2,8)).getOrElse(Nil) == 9)
    assert(Exercises.variance(Seq()) == None)
    assert(Exercises.variance(Seq()).getOrElse(42) == 42)
  }

  test("map2") {
    assert(Exercises.map2(Some(1), Some(2))(_+_).getOrElse(Nil) == 3)
    assert(Exercises.map2(Some(1), None)(_+_) == None)
    assert(Exercises.map2(None, Some(2))((_,_)) == None)
  }

  test("sequence") {
    assert(Exercises.sequence(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(Exercises.sequence(List(None, Some(2))) == None)
  }

  test("traverse") {
    assert(Exercises.traverse(List(1,2))(x => if (x % 2 == 0) Some(x) else None) == None)
    assert(Exercises.traverse(List(Nil,2))(x => Some(x)) == Some(List(Nil, 2)))
  }

  test("sequence2") {
    assert(Exercises.sequence2(List(Some(1), Some(2))) == Some(List(1,2)))
    assert(Exercises.sequence2(List(None, Some(2))) == None)
    assert(Exercises.sequence2(List(Some(1), Some(2), None)) == None)
  }
}
