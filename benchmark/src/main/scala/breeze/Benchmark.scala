package breeze.benchmark

import scala.reflect.ClassTag
import scala.annotation.tailrec
import scala.{specialized => spec}
import scala.util.Random
import Random._

import com.google.caliper.Runner
import com.google.caliper.SimpleBenchmark
import com.google.caliper.Param

import java.lang.Math
import java.math.BigInteger

/**
 * Extend this to create an actual benchmarking class.
 */
trait BreezeBenchmark extends SimpleBenchmark {
  /**
   * Sugar to run 'f' for 'reps' number of times.
   */
  def run[A](reps:Int)(f: => A): A = {
    def loop(a: A, i: Int): A = if (i < reps) loop(f, i + 1) else a
    if (reps < 1) sys.error("!") else loop(f, 1)
  }
}

/**
 * Extend this to create a main object which will run 'cls' (a benchmark).
 */
abstract class MyRunner(val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}
