package breeze.inference.bp

import breeze.linalg._
import breeze.numerics._
import breeze.util.Encoder
import collection.immutable.BitSet

/**
 * Implements basic belief propagation for computing variable
 * marginals in graphical models.
 *
 * For more powerful stuff, you should probably use Factorie.
 * This is--imho--easier to use for "simple" problems.
 *
 * @author dlwh
 */
object BeliefPropagation {

  /**
   * The result object for BeliefPropagation, useful for getting information
   * about marginals and edge marginals
   * @param model the Factor model used to perform inference
   * @param beliefs
   * @param messages
   */
  case class Beliefs(model: Model,
                     beliefs: IndexedSeq[DenseVector[Double]],
                     messages: IndexedSeq[IndexedSeq[DenseVector[Double]]],
                     factorLogPartitions: IndexedSeq[Double]) {
    def marginalFor[T](v: Variable[T]): Counter[T, Double] = Encoder.fromIndex(v.domain).decode(beliefs(model.variableIndex(v)))


    /**
     * returns a factor representing the factor marginal for the given marginal.
     * That is, f(assignment) will give the marginal probability of any given assignment.
     *
     * If the factor is not in the original model, this still works, but it
     * doesn't mean much unless logApply returns 0.0 for all values.
     * @param f the factor
     * @return the edge marginal factor
     */
    def factorMarginalFor(f: Factor): Factor = {
      if(f.variables.length == 1) {
        new Factor {
          val variables = f.variables
          val index = model.variableIndex(variables.head)

          def logApply(assignments: Array[Int]) = {
            math.log(beliefs(index)(assignments(0)))
          }
        }

      } else {
        new Factor {

          def variables = f.variables
          val fi = model.factorIndex(f)
          val divided = for( (v, m_fv) <- model.factorVariablesByIndices(fi) zip messages(fi)) yield {
            log(beliefs(v) :/ m_fv)
          }

          def logApply(assignments: Array[Int]) = {
            var ll = f.logApply(assignments)
            var i = 0
            while (i < divided.length) {
              ll += divided(i)(assignments(i))
              i += 1
            }
            ll -= factorLogPartitions(fi)
            ll
          }

        }
      }
    }
    // group the messages and then add them up
    private val messageContribution = {
      val acc = Encoder.fromIndex(model.variableIndex).tabulateArray(v => DenseVector.zeros[Double](v.size))
      for(fi <- 0 until model.factors.size; (v,m) <- model.factorVariablesByIndices(fi) zip messages(fi)) {
        acc(v) += log(m)
      }
      acc.map(softmax(_)).sum

    }

    val logPartition = factorLogPartitions.sum + messageContribution
  }

  /**
   * Performs inference on the model, giving a Beliefs object with marginals
   * @param model
   * @param maxIterations
   * @param tolerance
   * @return
   */
  def infer(model: Model, maxIterations: Int = 10, tolerance: Double = 1E-4) = {
    val beliefs = model.variables.map{ v =>
      val b = DenseVector.ones[Double](v.domain.size)
      b /= b.size.toDouble
      b
    }

    val messages = model.factors.map{ f =>
      f.variables.map { v => DenseVector.ones[Double](v.domain.size) }
    }

    val partitions =  new Array[Double](model.factors.size)

    // TODO:    go ahead and apply arity-1 factors. We'll only need to touch them once more to fix partitions
    val oneVariableFactors = BitSet.empty ++ (0 until model.factors.length).filter(i => model.factors(i).variables.length == 1)

    var touchedVariables = BitSet.empty

    /*
    for( f <- oneVariableFactors) {
      val vi = model.variableIndex(model.factors(f).variables(0))
      var partition = 0.0
      model.factors(f).foreachAssignment { ass =>
        var score = model.factors(f)(ass)
        messages(f)(0)(ass(0)) = score

        if(touchedVariables(vi))
          score *= beliefs(vi)(ass(0))

        partition += score
        beliefs(vi)(ass(0)) = score
      }
      beliefs(vi) /= partition
      touchedVariables += vi
    }
    */

    var converged = false
    var iter = 0

    val otherFactors = BitSet.empty ++ (0 until model.factors.length) -- oneVariableFactors

    while(!converged && iter < maxIterations) {
      converged = true
      for(f <- 0 until model.factors.length) {
        // localize the old beliefs and divide out the messages
        val divided = for( (v, m_fv) <- model.factorVariablesByIndices(f) zip messages(f)) yield {
          beliefs(v) :/ m_fv
        }

        val (newBeliefs, partition) = model.factors(f)._updateBeliefs(divided)

        // normalize new beliefs
        // compute new messages, store new beliefs in old beliefs
        for ( (globalV, localV) <- model.factorVariablesByIndices(f).zipWithIndex) {
          converged &&= (norm(beliefs(globalV) - newBeliefs(localV), inf) < 1E-4)
          if(!converged) {
            beliefs(globalV) := newBeliefs(localV)
            val mfv = messages(f)(localV)
            mfv := (newBeliefs(localV) / divided(localV))

            // nans are usually from infinities or division by 0.0, usually we can s
            for(i <- 0 until mfv.length) { if(mfv(i).isInfinite || mfv(i).isNaN) mfv(i) = 1.0}
          }
        }

        if(!converged)
          partitions(f) = math.log(partition)
      }

      iter += 1
    }


    new Beliefs(model, beliefs, messages, partitions)
  }
}
