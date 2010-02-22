package scalanlp.optimize


/**
 * Algorithm to find a maximum cost matching on a bipartite graph.
 * 
 * Implements the hungarian algorithm.
 * 
 * @author dlwh
 */
object KuhnMunkres extends BipartiteMatching {
  // based on http://www.enseignement.polytechnique.fr/informatique/INF441/INF441b/code/kuhnMunkres.py
  /**
   * Given a matrix of positive weights, finds the minimum weight bipartite matching between to arrays.
   * Returns a matching from the rows (the first index into the matrix) to the columns
   * (-1 for unmatched rows in the case of unbalanced entries) along
   * with the total score of the matching.
   */
  def extractMatching(weights: Seq[Seq[Double]]) = {
    val size = weights.length max weights(0).length;
    val arr = Array.tabulate(size,size) { (x,y) =>
      if(x < weights.length && y < weights(x).length) weights(x)(y) else 0.0;
    }
    for( row <- arr;
         min = row.min;
         i <- 0 until size) {
       row(i) -= min; 
    }
    for(
      c <- 0 until size;
      min = (0 until size) map { r => arr(r)(c) } min;
      r <- 0 until size
    ) {
       arr(r)(c) -= min; 
    }

    // find initial zeros
    val xyMatches = Array.fill(size)(-1);
    val xCovered = new collection.mutable.BitSet(size);
    val yxMatches = Array.fill(size)(-1);
    val yCovered = new collection.mutable.BitSet(size);
    val primes = Array.fill(size)(-1);
    initialize(); 
    coverZeros();

    while(yCovered.size < size) {

      var zero: Option[(Int,Int)] = nextZero;
      // find a zero
      while(!zero.isEmpty) {
        val root@(x,y) = zero.get;
        primes(x) = y;
        val oldY = xyMatches(x);
        if (oldY == -1) {
          alternatingPath(root);
          java.util.Arrays.fill(primes,-1);
          xCovered.clear();
          yCovered.clear();
          coverZeros();
        } else {
          xCovered += x;
          yCovered -= oldY;
        }

        zero = nextZero;
      }

      if(yCovered.size < size) {
        val minValue = (for {
          x <- 0 until size;
          if !xCovered(x)
          y <- 0 until size;
          if !yCovered(y)
        } yield arr(x)(y)).min;

        for(x <- 0 until size if xCovered(x); y <- 0 until size) {
          arr(x)(y) += minValue
        }

        for(y <- 0 until size if !yCovered(y); x <- 0 until size) {
          arr(x)(y) -= minValue
        }
      }
    }

    // BEGIN SUPPORT METHODS
    def initialize() {
      for (x <- 0 until size;
           y <- 0 until size;
           if xyMatches(x) == -1 && yxMatches(y) == -1 && arr(x)(y).abs < 1E-5) {
        xyMatches(x) = y;
        yxMatches(y) = x;
      }
    }

    def coverZeros() {
      for ( y <- 0 until size) {
        if(yxMatches(y) != -1) yCovered += y;
        else yCovered -= y;
      }
    }

    def nextZero = {
      val iter = (for {
        x <- 0 until size iterator;
        if !xCovered(x)
        y <- 0 until size iterator;
        if arr(x)(y).abs < 1E-5 && !yCovered(y)
      } yield (x,y));
      if(iter.hasNext) Some(iter.next);
      else None;
    }

    def alternatingPath(root: (Int,Int)) {
      var (x,y) = root;
      val path = collection.mutable.Set[(Int,Int)]();
      path += (x -> y);
      while(y != -1 && yxMatches(y) != -1) {
        x = yxMatches(y);
        path += (x->y);
        y = primes(x);
        if(y != -1) {
          path += (x -> y);
        }
      }

      for( (xx,yy) <- path) {
        if(yxMatches(yy) == xx) {
          yxMatches(yy) = -1;
          xyMatches(xx) = -1
        }
        if(primes(xx) == yy) {
          xyMatches(xx) = yy;
          yxMatches(yy) = xx;
        }
      }
    }
    // END SUPPORT METHODS


    val xResult = Array.fill(weights.length)(-1)
    for( (x,y) <- yxMatches.zipWithIndex if x < weights.length) {
      assert(xResult(x) == -1);
      xResult(x) = y;
    }
    assert(xResult.count(_ != -1) == weights(0).length.min(xResult.length));

    val cost = xyMatches.zipWithIndex.iterator map { case (y,x) =>
      if(x < weights.length && y < weights(x).length && y != -1) weights(x)(y)
      else 0.0
    } reduceLeft(_+_);
    (xResult toSeq,cost);

  }
}
