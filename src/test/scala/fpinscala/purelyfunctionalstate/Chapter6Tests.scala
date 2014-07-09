package fpinscala.purelyfunctionalstate

import org.scalatest.FunSuite

/**
 * Created by lucho on 09/07/14.
 */
class Chapter6Tests extends FunSuite {

  test("randomPair") {
    println(RNG.randomPair(RNG.simple(5)))
  }

  test("positiveInt") {
    val rng = RNG.simple(5)
    val (i,rng2) = RNG.positiveInt(rng)
    println(i, RNG.positiveInt(rng2))
  }

  test("double") {
    val rng = RNG.simple(5)
    val (d,rng2) = RNG.double(rng)
    println(d, RNG.double(rng2))
  }

  test("intDouble") {
    val rng = RNG.simple(5)
    println(RNG.intDouble(rng))
  }

  test("doubleInt") {
    val rng = RNG.simple(5)
    println(RNG.doubleInt(rng))
  }

  test("double3") {
    val rng = RNG.simple(5)
    println(RNG.double3(rng))
  }

  test("ints") {
    val rng = RNG.simple(5)
    println(RNG.ints(3)(rng))
  }

  test("positiveMax") {
    val rng = RNG.simple(5)
    val positiveMax5 = RNG.positiveMax(5)
    println(positiveMax5(rng))
  }

  test("double2") {
    val rng = RNG.simple(5)
    val (d, rng2) = RNG.double2(rng)
    println(d, RNG.double2(rng2))
  }

  test("intDouble2") {
    val rng = RNG.simple(5)
    println(RNG.intDouble2(rng))
  }

  test("doubleInt2") {
    val rng = RNG.simple(5)
    println(RNG.doubleInt2(rng))
  }

  test("sequence") {
    val rng = RNG.simple(5)
    val (l, rng2) = RNG.sequence(List(RNG.int, RNG.double22, RNG.int))(rng)
    println(l, RNG.sequence(List(RNG.double22, RNG.double22, RNG.int))(rng2))
  }

  test("positiveInt2") {
    val rng = RNG.simple(5)
    val (i,rng2) = RNG.positiveInt2(rng)
    println(i, RNG.positiveInt2(rng2))
  }

  test("positiveMaxa") {
    val rng = RNG.simple(5)
    val positiveMax5 = RNG.positiveMaxa(5)
    val (v, rng2) = positiveMax5(rng)
    println(v, positiveMax5(rng2))
  }

  test("intDouble2a") {
    val rng = RNG.simple(5)
    println(RNG.intDouble2a(rng))
  }

}
