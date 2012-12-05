package breeze.cluster

import breeze.math.{MutableInnerProductSpace, MutableNormedSpace}
import breeze.numerics._
import breeze.linalg._
import breeze.util._
import breeze.util.Implicits._
import collection.mutable.ArrayBuffer
import breeze.stats.distributions.Multinomial

/**
 * Implements the KMeans++ algorithm for clustering points in a normed space.
 * @author dlwh
 */
class KMeans[T](k: Int, tolerance: Double = 1E-4)(implicit space: MutableInnerProductSpace[T, Double]) {
  import space._
  case class State(means: IndexedSeq[T], error: Double, previousError: Double=Double.PositiveInfinity) {
    def converged = closeTo(error,previousError,tolerance)
  }


  def cluster(points: IndexedSeq[T]) = {
    iterates(points).last
  }

  def iterates(points: IndexedSeq[T]) = {
    if(points.length <= k) Iterator(State(points, 0.0))
    else {
      Iterator.iterate(initialState(points)){ current =>
        val distances = points.par.map { x =>
          val distances = current.means.map{m => distance(x,m)}
          val minPoint = distances.argmin // which cluster minimizes euclidean distance
          (x, minPoint,distances(minPoint))
        }

        val newClusters = distances.groupBy(_._2).seq.values.map{_.map(_._1)}
        val newMeans = newClusters.par.map(clust => clust.foldLeft(zeros(clust.head))(_ += _) /= clust.size.toDouble)
        val error = distances.map(tuple => math.pow(tuple._3, 2)).sum

        State(newMeans.seq.toIndexedSeq, error, current.error)
      }.takeUpToWhere(state => state.converged)
    }


  }

  private def distance(a: T, b: T) =  norm(a - b)

  private def initialState(points: IndexedSeq[T]):State = {
    val probs = Array.fill(points.length)(1.0)
    val means = new ArrayBuffer[T]()

    while(means.length < k) {
      val newMean = points(Multinomial(new DenseVector(probs)).sample())
      means += newMean
      for(i <- (0 until probs.length).par) {
        probs(i) = math.min(probs(i), math.pow(distance(newMean, points(i)),2))
      }
    }

    new State(means, probs.sum)
  }

}
