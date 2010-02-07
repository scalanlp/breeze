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
    require(weights forall (_.forall(_ >= 0)));
    val size = weights.length max weights(0).length;
    val arr = Array.tabulate(size,size) { (x,y) =>
      if(x < weights.length && y < weights(x).length) weights(x)(y) else 0.0;
    }
    val xLabels = arr.map { _ min }
    val yLabels = Array.fill(size)(0.0);

    def slack(x: Int, y: Int) = xLabels(x) + yLabels(y) - arr(x)(y);

    val xyMatches = collection.mutable.Map[Int,Int]();
    val yxMatches = collection.mutable.Map[Int,Int]();

    while(xyMatches.size < size) {
      // first free var
      val freeVar = 0 until size filter (!xyMatches.contains (_)) head;
      augment(freeVar);
    }

    def augment(root: Int) {
      val xTree = collection.mutable.Set[Int]();
      xTree += root;
      var yTree = Map[Int,Int]();
      val maxSlack = for( y <- Array.range(0,size) ) yield (slack(root,y),root);
      while(true) {
        val ( (w,x),y) = (for ( y <- 0 until size if !yTree.contains(y)) yield (maxSlack(y),y)).max
        if(w != 0) improveLabels(w,xTree, yTree, maxSlack);
        //assert(slack(x,y).abs < 1E-8,slack(x,y));
        yTree += (y -> x);
        if(yxMatches contains y) {
          val oldX = yxMatches(y);
          assert(!xTree.contains(oldX))
          xTree += oldX;
          val oldSlack = slack(oldX,y);
          for( y <- 0 until size if !yTree.contains(y) && maxSlack(y)._1 < oldSlack) {
            maxSlack(y) = (slack(oldX,y), oldX);
          }
        } else {
          improve(y, yTree)
          return;
        }
      }
    }

    def improveLabels(w: Double, xTree: collection.Set[Int], yTree: Map[Int,Int], maxSlack: Array[(Double,Int)]) {
      for( x <- xTree) xLabels(x) -= w;
      for( y <- 0 until size)
        if(yTree.contains(y))
          yLabels(y) += w
        else {
          maxSlack(y) = maxSlack(y).copy(_1 = maxSlack(y)._1 - w)
        }
    }

    def improve(y: Int, yTree: Map[Int,Int]) {
      val x = yTree(y);
      if(xyMatches contains x) improve(xyMatches(x),yTree);
      xyMatches(x) = y
      yxMatches(y) = x;
    }

    val xResult = Array.tabulate(weights.length) { x =>
      val y = xyMatches(x)
      if(y >= weights(0).length) -1
      else y;
    }

    val cost = xLabels.reduceLeft(_+_) + yLabels.reduceLeft(_+_);
    (xResult toSeq,cost);

  }
}
