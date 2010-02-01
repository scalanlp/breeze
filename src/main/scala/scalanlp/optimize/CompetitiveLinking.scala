package scalanlp.optimize

import collection.mutable.BitSet;

object CompetitiveLinking {

  /**
   * Performs greedy bipartite matching (aka competitive linking).
   * @params matchingPotentials <n,m> matrix, m >= n
   * @return matching of size n, with values in 0..m-1, along with the score of the matching
   */
  def extractMatching(matchingPotentials: Seq[Seq[Double]]):(Seq[Int],Double) = {
    val n = matchingPotentials.length;
    val m = matchingPotentials(0).length;
    require (m >= n, "Column dimension must be at least the size of row dim.");
    val predsIt = for( (arr,i) <- matchingPotentials.iterator.zipWithIndex;
                      (w,j) <- arr.iterator.zipWithIndex) yield Prediction(i,j,w);
    val preds = predsIt.toSeq.sortWith(_.v > _.v);
    val leftSet = new BitSet(n);
    val rightSet = new BitSet(m);
    val matching = Array.fill(n)(-1);
    var score = 0.0;
    for (pred <- preds.iterator.takeWhile(_ => leftSet.size < n || rightSet.size < m);
         if !leftSet(pred.i) && !rightSet(pred.j)) {
      matching(pred.i) = pred.j;
      leftSet.add(pred.i);
      rightSet.add(pred.j);
      score += pred.v;
    }
    matching foreach { a => assert(a >= 0) };
    (matching,score);
  }

  private case class Prediction(i: Int, j: Int, v: Double);
}
