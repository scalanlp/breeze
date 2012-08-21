package breeze.inference.bp

import org.scalatest._
import breeze.numerics.I

class BeliefPropagationTest extends FunSuite {
  test("Simple Test") {
    val v1, v2, v3, v4 = Variable(0 to 1)

    // based on factorie loop4 test
    val factors = IndexedSeq(
      Factor.fromLogFn(v4)(i => 5 * I(i == 0)),
      Factor.fromLogFn(v1, v2)( (i,j) => if(i == j) -1 else 0.0 ),
      Factor.fromLogFn(v1, v3)( (i,j) => if(i == j) -1 else 0.0 ),
      Factor.fromLogFn(v2, v4)( (i,j) => if(i == j) -1 else 0.0 ),
      Factor.fromLogFn(v3, v4)( (i,j) => if(i == j) -1 else 0.0 )
    )

    val model = Model(IndexedSeq(v1, v2, v3, v4), factors)
    val inf = BeliefPropagation.infer(model)
    assert(inf.beliefs(0)(0) >= 0.999, inf.beliefs(0))
    assert(inf.beliefs(0)(1) < 1E-3)

    assert(inf.beliefs(1)(0) < 1E-3)
    assert(inf.beliefs(1)(1) >= 0.999)

    assert(inf.beliefs(2)(0) < 1E-3)
    assert(inf.beliefs(2)(1) >= 0.999)

    assert(inf.beliefs(3)(0) >= 0.999)
    assert(inf.beliefs(3)(1) < 1E-3)

  }

}
