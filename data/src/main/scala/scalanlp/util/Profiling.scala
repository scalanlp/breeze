package scalanlp.util

/*
 Copyright 2009 David Hall, Daniel Ramage
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at 
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License. 
*/


object Profiling {

  /**
   * Returns the average time to execute n iterations of
   * the given function in ms.
   */
  def time(n : Int)(function : (() => Any)) : Double = {
    var total = 0.0; 
    for (i <- 0 until n) {
      val start = System.currentTimeMillis;
      function();
      total += (System.currentTimeMillis - start);
    }
    return total / n;
  }
  
  
  def main(args : Array[String]) {
    val n = 1000000;
    
    def loop1 = {
      var sum = 0;
      for (i <- 0 until n) { sum += i; }
      sum;
    }
    
    def loop2 = {
      var sum = 0;
      var i = 0;
      while (i < n) { sum += i; i += 1; }
      sum;
    }
    
    var iters = 100;
    println("From range:  " + time(iters)(loop1 _));
    println("Manual loop: " + time(iters)(loop2 _));
  }
}
