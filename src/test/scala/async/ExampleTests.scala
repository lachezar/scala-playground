package async

import org.scalatest.FunSuite

/**
 * Created by lucho on 10/07/14.
 */
class ExampleTests extends FunSuite {

  test("simple test 1") {
      Example.run
      Thread.sleep(10000)
  }

  test("parallel test 1") {
    Example.runParallel
    Thread.sleep(100000)
  }
}
