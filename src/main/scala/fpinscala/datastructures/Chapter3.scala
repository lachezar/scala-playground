/**
 * Created by lucho on 06/07/14.
 */

package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail(xs: List[_]): List[_] = xs match {
    case Nil => Nil
    case Cons(h, t) => t
  }

  def drop[A](xs: List[A], n: Int): List[A] = (xs, n) match {
    case (l, 0) => l
    case (Nil, _) => Nil
    case (Cons(h, t), _) => drop(t, n-1)
  }

  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) => {
      if (f(h)) Cons(h, dropWhile(t)(f))
      else dropWhile(t)(f)
    }
  }

  def setHead[A](xs: List[A], n: A): List[A] = xs match {
    case Nil => Nil
    case Cons(h, t) => Cons(n, t)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  
  def foldRight[A,B](l: List[A], _agr: B)(f: (A, B) => B): B = l match {
    case Nil => _agr
    case Cons(h, t) => f(h, foldRight(t, _agr)(f))
  }

  def sum2(l: List[Int]) = {
    foldRight(l, 0)(_+_)
  }

  def sum3(l: List[Double]) = {
    foldRight(l, 0.0)(_+_)
  }

  def product2(l: List[Double]) = {
    foldRight(l, 1.0)(_*_)
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_, a) => a + 1)
  }

  def foldLeft[A,B](l: List[A], _agr: B)(f: (A, B) => B): B = l match {
    case Nil => _agr
    case Cons(h, t) => foldLeft(t, f(h, _agr))(f)
  }

  def sumfl(l: List[Int]) = {
    foldLeft(l, 0)(_+_)
  }

  def productfl(l: List[Double]) = {
    foldLeft(l, 1.0)(_*_)
  }

  def lengthfl[A](l: List[A]): Int = {
    foldLeft(l, 0)((_, a) => a + 1)
  }

  def reverse[A](l: List[A]) = {
    foldLeft(l, Nil:List[A])(Cons(_,_))
  }

  def append2[A](l1: List[A], l2: List[A]) = {
    foldRight(l1, l2)(Cons(_,_))
  }

  def append3[A](l1: List[A], l2: List[A]) = {
    _append3(reverse(l1), l2)
  }

  def _append3[A](l1: List[A], l2: List[A]) = {
    foldLeft(l1, l2)(Cons(_,_))
  }

  def concat[A](ll: List[List[A]]): List[A] = ll match {
    case Nil => Nil
    case Cons(h, Nil) => h
    case Cons(h, Cons(h2, t)) => concat(Cons(append3(h, h2), t))
  }

  def plus1(l: List[Int]): List[Int] = {
    reverse(foldLeft(l, Nil:List[Int])((a,b) => Cons(a + 1, b)))
  }

  def map[A, B](l: List[A])(f: A => B): List[B] = {
    reverse(foldLeft(l, Nil:List[B])((a, b) => Cons(f(a), b)))
  }

  def stringify(l: List[Double]): List[String] = {
    map(l)("" + _)
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] = {
    reverse(foldLeft(l, Nil:List[A])(
      (a, b) =>
        if (f(a)) Cons(a, b)
        else b
    ))
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = {
    concat(map(l)(f))
  }

  def filter2[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(x =>
      if (f(x)) List(x)
      else List()
    )
  }

  def zip[A, B](l1: List[A], l2: List[A])(f: (A,A) => B): List[B] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zip(t1, t2)(f))
  }

  def hasSubsequence[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) => {
      if (!hasMatch(l1, l2)) {
        hasSubsequence(t1, l2)
      } else {
        true
      }
    }
  }

  def hasMatch[A](l1: List[A], l2: List[A]): Boolean = (l1, l2) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && hasMatch(t1, t2)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }

  def maximum(t: Tree[Int], mv: Int = Int.MinValue): Int = t match {
    case Leaf(v) => v max mv
    case Branch(l, r) => maximum(l, mv) max maximum(r, mv) max mv
  }

  def depth[A](t: Tree[A], md: Int = 0): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}
