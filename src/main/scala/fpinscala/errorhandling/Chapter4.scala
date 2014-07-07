package fpinscala.errorhandling

/**
 * Created by lucho on 07/07/14.
 */

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(get))
  def flatMap[B](f: A => Option[B]): Option[B] = f(get)
  def getOrElse[B >: A](default: => B): B = get
  def orElse[B >: A](ob: => Option[B]): Option[B] = this
  def filter(f: A => Boolean): Option[A] = if (f(get)) this else None
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[Nothing] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob
  def filter(f: Nothing => Boolean): Option[Nothing] = None
}

object Exercises {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs)
    m.flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
   a.flatMap(x => {
     b.map(y => {
       f(x, y)
     })
   })
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(List()): Option[List[A]])(map2(_,_)(_::_))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight(Some(List()): Option[List[B]])((e,z) => map2(f(e),z)(_::_))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(x => x)
  }
}
