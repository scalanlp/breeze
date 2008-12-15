package scalanlp.util

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
