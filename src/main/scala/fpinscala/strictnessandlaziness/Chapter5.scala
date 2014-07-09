package fpinscala.strictnessandlaziness

/**
 * Created by lucho on 08/07/14.
 */

trait Stream[+A] {
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def toList: List[A] = uncons match {
    case None => Nil
    case Some((h, t)) => h :: t.toList
  }

  def take(n: Int): Stream[A] = n match {
    case 0 => Stream()
    case _ => uncons match {
      case None => Stream()
      case Some((h,t)) => Stream.cons(h, t.take(n - 1))
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = uncons match {
    case Some((h,t)) if p(h) => Stream.cons(h, t.takeWhile(p))
    case _ => Stream()
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => {
      println("called")
      p(a) || b
    })

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => {
      println("called")
      p(a) && b
    })

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(Stream[A]())((a, b) => {
      if (p(a)) {
        println("called")
        Stream.cons(a, b)
      }
      else Stream()
    })
  }

  //map, filter, flatmap,append

  def map[B](f: A => B): Stream[B] = {
    foldRight(Stream[B]())((a,b) => {
      println("called")
      Stream.cons(f(a), b)
    })
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(Stream[B]())((a,b) => {
      f(a).append(b)
    })
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)(Stream.cons(_, _))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(Stream[A]())((a,b) => {
      if (p(a)) {
        Stream.cons(a, b)
      } else {
        b
      }
    })
  }

  def zip[B >: A](s2: Stream[B]): Stream[(B,B)] = {
    (this.uncons, s2.uncons) match {
      case (Some((h,t)), Some((h2,t2))) => Stream.cons((h,h2), t.zip(t2))
      case _ => Stream()
    }
  }

  def zipAll[B, C >: A, D >: B](that : Stream[B], thisElem : C, thatElem : D) : Stream[(C, D)] = {
    (this.uncons, that.uncons) match {
      case (Some((h,t)), Some((h2,t2))) => Stream.cons((h,h2), t.zipAll(t2, thisElem, thatElem))
      case (Some((h,t)), None) => Stream.cons((h,thatElem), t.zipAll(Stream(), thisElem, thatElem))
      case (None, Some((h2,t2))) => Stream.cons((thisElem,h2), Stream().zipAll(t2, thisElem, thatElem))
      case _ => Stream()
    }
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this.zipAll(s, None, None).forAll({
      _ match {
        case (_, None) => true
        case (None, b) => false
        case (a, b) => a == b
      }
    })
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this)(s => {
      if (s.isEmpty) None
      else Some((s, s.drop(1)))
    }).append(Stream(Stream()))
  }

  def drop(n: Int): Stream[A] = uncons match {
    case None => Stream()
    case Some((h,t)) => {
      n match {
        case 0 => this
        case _ => t.drop(n - 1)
      }
    }
  }
}

object Stream {
  def empty[A]: Stream[A] =
    new Stream[A] {
      def uncons = None
    }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fib(n: Int = 0, prev: Int = 1): Stream[Int] = {
    cons(n, fib(n + prev, n))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,ns)) => cons(a, unfold(ns)(f))
      case None => Stream()
    }
  }

  def ones2[A]: Stream[Int] = unfold(Nil)(_ => Some((1, Nil)))

  def constant2[A](a: A): Stream[A] = unfold(Nil)(_ => Some((a, Nil)))

  def from2(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))

  def fib2: Stream[Int] = cons(0, unfold((0,1))(x => Some((x._2, (x._2, x._1 + x._2)))))

  def hasSubsequence[A](s1: Stream[A], s2: Stream[A]): Boolean = s1.tails.exists(_.startsWith(s2))
}