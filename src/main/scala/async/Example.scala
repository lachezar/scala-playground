package async

import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * Created by lucho on 10/07/14.
 */
object Example {

  def fib(n: Int): Int = n match {
    case 1 => 1
    case 2 => 1
    case _ => fib(n-1) + fib(n-2)
  }

  def run = {
    val p = Promise[Int]
    val f = p.future
    val producer = Future {
      val r = fib(38)
      p success r
      println("producer", r)
    }
    val consumer = Future {
      f onSuccess {
        case r => println("consumer", r)
      }
    }
  }

  def runParallel = {
    val fut1 = Future{ fib(45) }
    val fut2 = Future{ fib(45) }
    val fut3 = Future{ fib(45) }
    val fut4 = Future{ fib(45) }
    val fut5 = Future{ fib(45) }

    val aggFut = for{
      f1Result <- fut1
      f2Result <- fut2
      f3Result <- fut3
      f4Result <- fut4
      f5Result <- fut5
    } yield (f1Result, f2Result, f3Result, f4Result, f5Result)

    aggFut.onSuccess {
      case (r1, r2, r3, r4, r5) => println(r1, r2, r3, r4, r5)
    }

  }
}
