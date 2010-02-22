package scalanlp.optimize

/**
 * Algorithms for finding a bipartite matching.
 * We include one optimal algorithm (KuhnMunkres)
 * and one greedy algorithm (competitive linking).
 *
 * Algorithms find <b> minimum </b> matchings.
 * 
 */
trait BipartiteMatching {
  def extractMatching(weights: Seq[Seq[Double]]): (Seq[Int],Double);
}
