package breeze.inference.bp

import org.scalatest._
import breeze.numerics.{I,closeTo}
import breeze.linalg.DenseMatrix

class BeliefPropagationTest extends FunSuite {
  test("Simple Test") {
    import SimpleProblem._
    assert(closeTo(inf.beliefs(0)(0),marginals(0), 3E-3), inf.beliefs(0).toString + " " + marginals.mkString("{",", ", "}"))
    assert(closeTo(inf.beliefs(1)(0),marginals(1), 4E-3), inf.beliefs(1).toString + " " + marginals.mkString("{",", ", "}"))
    assert(closeTo(inf.beliefs(2)(0),marginals(2), 3E-3), inf.beliefs(2).toString + " " + marginals.mkString("{",", ", "}"))
    assert(closeTo(inf.beliefs(3)(0),marginals(3), 3E-3), inf.beliefs(3).toString + " " + marginals.mkString("{",", ", "}"))

    assert(closeTo(math.log(part), inf.logPartition, 1E-3), math.log(part) + " " + inf.logPartition + " " + inf.factorLogPartitions.mkString(", "))
  }

  test("Simple Test Factor Marginals") {
    import SimpleProblem._

    val inferredMarginals = factors.map(inf.factorMarginalFor _)
    for( (guess,gold) <- inferredMarginals zip factorMarginals) {
      guess.foreachAssignment { ass =>
        if(ass.length == 1) {
          assert(closeTo(guess(ass),gold(ass(0),ass(0)), 1E-2))
        } else {
          assert(closeTo(guess(ass),gold(ass(0),ass(1)), 1E-2), guess.variables + " " + guess(ass) + " " +gold(ass(0),ass(1))  + " " + ass.mkString(" "))
        }

      }
    }

  }

  object SimpleProblem {
    val v1, v2, v3, v4 = Variable(0 to 1)

    // based on factorie loop4 test
    val factors = IndexedSeq(
      Factor.fromLogFn(v1, v2)( (i,j) => -I(i == j)),
      Factor.fromLogFn(v1, v3)( (i,j) => -I(i == j)),
      Factor.fromLogFn(v2, v4)( (i,j) => -I(i == j)),
      Factor.fromLogFn(v3, v4)( (i,j) => -I(i == j)),
      Factor.fromLogFn(v4)(i => 5 * I(i == 0))
    )

    val joint: Factor = factors.reduceLeft(_ * _)
    val part = math.exp(joint.logPartition)
    val marginals = Array(0.0, 0.0, 0.0, 0.0)
    val factorMarginals = Array.fill(factors.length)(DenseMatrix.zeros[Double](2,2))

    joint.foreachAssignment { ass =>
      val score = joint(ass)
      val marg: Double = score / part
      if(ass(0) == 0) marginals(0) += marg
      if(ass(1) == 0) marginals(1) += marg
      if(ass(2) == 0) marginals(2) += marg
      if(ass(3) == 0) marginals(3) += marg

      factorMarginals(0)(ass(0),ass(1)) += marg
      factorMarginals(1)(ass(0),ass(2)) += marg
      factorMarginals(2)(ass(1),ass(3)) += marg
      factorMarginals(3)(ass(2),ass(3)) += marg
      factorMarginals(4)(ass(3),ass(3)) += marg
    }

    val model = Model(IndexedSeq(v1, v2, v3, v4), factors)
    val inf = BeliefPropagation.infer(model)
  }

}
