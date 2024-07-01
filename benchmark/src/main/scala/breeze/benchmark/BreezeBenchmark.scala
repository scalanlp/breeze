/*
 *
 *  Copyright 2015 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.benchmark

import com.google.caliper.Benchmark
import com.google.caliper.runner.CaliperMain

/**
 * Extend this to create an actual benchmarking class.
 */
trait BreezeBenchmark {

  /**
   * Sugar to run 'f' for 'reps' number of times.
   */
  def run[A](reps: Int)(f: => A): A = {
    if (reps < 1) sys.error("!")
    var i = 0
    var result: Option[A] = None
    while (i < reps) {
      result = Some(f)
      i += 1
    }
    result.get
  }

  def runWith[A, B](reps: Int, constructor: => B)(f: B => A): A = {
    if (reps < 1) sys.error("!")
    var i = 0
    var result: Option[A] = None
    val obj: B = constructor
    while (i < reps) {
      result = Some(f(obj))
      i += 1
    }
    result.get
  }

  def runWith2[A, B, C](reps: Int, constructor: => B, constructor2: => C)(f: (B, C) => A): A = {
    if (reps < 1) sys.error("!")
    var i = 0
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
abstract class MyRunner(val cls: java.lang.Class[_]) {
  def main(args: Array[String]): Unit = CaliperMain.main(cls, args)
}
