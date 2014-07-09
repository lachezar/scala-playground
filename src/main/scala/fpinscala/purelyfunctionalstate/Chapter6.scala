package fpinscala.purelyfunctionalstate

/**
 * Created by lucho on 09/07/14.
 */

trait RNG {
  def nextInt: (Int, RNG)
}

case class State[S,+A](run: S => (A,S)) {

  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG, A]

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (va, rng2) = f(rng)
      val h = g(va)
      h(rng2)
    }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a =>
      rng => {
        val (va, rng2) = s(rng)
        (f(va), rng2)
      }
    )
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a =>
      rng => {
        flatMap(rb)(b =>
          rng2 => {
            (f(a,b), rng2)
          }
        )(rng)
      }
    )
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs match {
      case Nil => (List(), rng)
      case h :: t => {
        val (e, rng2) = h(rng)
        val (l, rng3) = sequence(t)(rng2)
        (e :: l, rng3)
      }
    }
}

object RNG {

  type State[S,+A] = S => (A,S)
  type Rand[A] = State[RNG, A]

  val int: Rand[Int] = _.nextInt
  val double22: Rand[Double] = map(RNG.int)(_.toDouble)

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (i1,rng2) = rng.nextInt
    val (i2,rng3) = rng2.nextInt
    ((i1,i2), rng3)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    i match {
      case Int.MinValue => rng2.nextInt
      case _ => (math.abs(i), rng2)
    }
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0) (i.toDouble / Int.MinValue, rng2)
    else (i.toDouble / Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = RNG.double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i,d), rng2) = intDouble(rng)
    ((d,i), rng2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) (List(), rng)
    else {
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(count - 1)(rng2)
      (i :: l, rng3)
    }
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def positiveMax(n: Int): Rand[Int] = map(RNG.positiveInt)(_ % n)

  def double2(rng: RNG): (Double, RNG) = map(RNG.int)(_.toDouble)(rng)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a,b), rng3)
    }
  }

  def intDouble2(rng: RNG): ((Int,Double), RNG) = map2(RNG.int, RNG.double2)((_,_))(rng)
  def doubleInt2(rng: RNG): ((Double,Int), RNG) = map2(RNG.double2, RNG.int)((_,_))(rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs match {
      case Nil => (List(), rng)
      case h :: t => {
        val (e, rng2) = h(rng)
        val (l, rng3) = sequence(t)(rng2)
        (e :: l, rng3)
      }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (va, rng2) = f(rng)
      val h = g(va)
      h(rng2)
    }

  def positiveInt2: Rand[Int] = {
    flatMap(RNG.int)(a =>
      rng =>
        if (a == Int.MinValue) RNG.int(rng)
        else (math.abs(a), rng)
    )
  }

  def mapa[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a =>
      rng => {
        val (va, rng2) = s(rng)
        (f(va), rng2)
      }
    )
  }

  def positiveMaxa(n: Int): Rand[Int] = mapa(RNG.positiveInt)(_ % n)

  def map2a[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a =>
      rng => {
        flatMap(rb)(b =>
          rng2 => {
            (f(a,b), rng2)
          }
        )(rng)
      }
    )
  }

  def intDouble2a(rng: RNG): ((Int,Double), RNG) = map2a(RNG.int, RNG.double2)((_,_))(rng)
}
