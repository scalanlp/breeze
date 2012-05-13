package scalanlp.inference


/**
 *
 * @author dlwh
 */
class ConcurrentExpectationPropagation[F, Q](project: (Q, F) => (Q, Double), criterion: Double = 1E-4)(implicit qFactor: Q <:< Factor[Q]) {

  case class State(f_~ : IndexedSeq[Q], q: Q, prior: Q, partitions: IndexedSeq[Double]) {
    lazy val logPartition = f_~.foldLeft(prior)(_ * _).logPartition + partitions.sum
  }

  def inference(prior: Q, f: IndexedSeq[F], initialF_~ : IndexedSeq[Q]): Iterator[State] = {
    val lastQ: Q = prior * initialF_~.par.reduce(_ * _)

    val initPartitions = IndexedSeq.fill(f.length)(Double.NegativeInfinity)

    // pass through the data
    val it: Iterator[State] = new Iterator[State] {
      var cur = State(initialF_~, lastQ, prior, initPartitions)
      var consumed = true

      def hasNext = !consumed || {
        val results = (0 until f.length).par.map { i =>
          val State(f_~, q, _, _) = cur
          val fi = f(i)
          val fi_~ = f_~(i)
          val q_\ = q / fi_~
          val (new_q, new_partition) = project(q_\, fi)
          val newF_~ = new_q / q_\
          newF_~ -> new_partition
        }
        val newPartitions = results.map(_._2).toIndexedSeq
        val newF_~ = results.map(_._1).toIndexedSeq
        val qNew: Q = prior * newF_~.par.reduce(_ * _)
        val hasNext:Boolean = (cur.q eq lastQ) || !qNew.isConvergedTo(cur.q, criterion)
        consumed = !hasNext
        cur = State(newF_~, qNew, prior, newPartitions)
        hasNext
      }

      def next() = {
        if (consumed) hasNext
        consumed = true
        cur
      }
    }

    it
  }


}


