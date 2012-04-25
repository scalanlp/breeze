package scalanlp.inference


/**
 *
 * @author dlwh
 */
class ConvexEP[F, Q](project: (Q, F, Double) => (Q, Double), criterion: Double = 1E-4)(implicit qFactor: Q <:< ExpFactor[Q]) {

  case class State(f_~ : IndexedSeq[Q], q: Q, prior: Q, partitions: IndexedSeq[Double]) {
    lazy val logPartition = f_~.foldLeft(prior)(_ * _).logPartition + partitions.sum
  }

  def inference(prior: Q,
                f: IndexedSeq[F],
                initialF_~ : IndexedSeq[Q]): Iterator[State] = {
    val lastQ: Q = initialF_~.foldLeft(prior)(_ * _)
    val power = 1.0/f.length

    val initPartitions = IndexedSeq.fill(f.length)(Double.NegativeInfinity)

    // pass through the data
    val it: Iterator[State] = new Iterator[State] {
      var cur = State(initialF_~, lastQ * (-lastQ.logPartition), prior, initPartitions)
      var consumed = true

      def hasNext = !consumed || {
        val next = (0 until f.length).iterator.foldLeft(cur) {
          (state, i) =>
            val State(f_~, q, _, partitions) = state
            val fi = f(i)
            val fi_~ = f_~(i)
            val q_\ = q / (fi_~ ** power)
            val (new_q, new_partition) = project(q_\, fi, power)
            val newFi_~ = fi_~ ** (1-power) * ((new_q / q_\) ** power)
            val newF_~ = f_~.updated(i, newFi_~)
            State(newF_~, new_q, prior, partitions.updated(i, new_partition))
        }
        val hasNext:Boolean = (cur.q eq lastQ) || !next.q.isConvergedTo(cur.q, criterion)
        consumed = !hasNext
        cur = next
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


