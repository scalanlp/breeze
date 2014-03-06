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
    if (reps < 1) sys.error("!")
    var i=0
    var result: Option[A] = None
    while (i < reps) {
      result = Some(f)
      i += 1
    }
    result.get
  }

  def runWith[A,B](reps: Int, constructor: =>B)(f: B=>A): A = {
    if (reps < 1) sys.error("!")
    var i=0
    var result: Option[A] = None
    val obj: B = constructor
    while (i < reps) {
      result = Some(f(obj))
      i += 1
    }
    result.get
  }

  def runWith2[A,B,C](reps: Int, constructor: =>B, constructor2: =>C)(f: (B,C)=>A): A = {
    if (reps < 1) sys.error("!")
    var i=0
    var result: Option[A] = None
    val obj1: B = constructor
    val obj2: C = constructor2
    while (i < reps) {
      result = Some(f(obj1, obj2))
      i += 1
    }
    result.get
  }
}

/**
 * Extend this to create a main object which will run 'cls' (a benchmark).
 */
abstract class MyRunner(val cls:java.lang.Class[_ <: com.google.caliper.Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}
