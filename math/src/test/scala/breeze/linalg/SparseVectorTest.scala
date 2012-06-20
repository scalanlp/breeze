package breeze.linalg

import org.scalatest._
import org.scalatest.junit._
import org.junit.runner.RunWith

/**
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class SparseVectorTest extends FunSuite {

  val TOLERANCE = 1e-4
  def assertClose(a : Double, b : Double) =
    assert(math.abs(a - b) < TOLERANCE, a + " vs. " +  b)

  test("MulInner") {
    val a = SparseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val b = SparseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bdSplit = DenseVector(0., 0.15951, 0., 0.83671,0., 0.56002, 0., 0.57797, 0., 0.54450)
    val bdd = bdSplit(1 to 9 by 2)
    assertClose(a dot b, .90249)
//    assertClose(a dot bd, .90249)
    assertClose(bd dot a, .90249)
    assertClose(bdd dot a, .90249)
  }

  test("Subtraction") {
    val a = SparseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val ad = DenseVector(0.56390,0.36231,0.14601,0.60294,0.14535)
    val b = SparseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bd = DenseVector(0.15951,0.83671,0.56002,0.57797,0.54450)
    val bdd = bd - ad
    b -= a
    bd -= a
    assertClose(b.norm(2), bd.norm(2))
    assertClose(bdd.norm(2), bd.norm(2))
  }


  test("Norm") {
    val v = SparseVector(-0.4326, -1.6656, 0.1253, 0.2877, -1.1465)
    assertClose(v.norm(1), 3.6577)
    assertClose(v.norm(2), 2.0915)
    assertClose(v.norm(3), 1.8405)
    assertClose(v.norm(4), 1.7541)
    assertClose(v.norm(5), 1.7146)
    assertClose(v.norm(6), 1.6940)
    assertClose(v.norm(Double.PositiveInfinity), 1.6656)
  }

  test("SV ops work as Vector") {
    val a = SparseVector(1.0, 2.0, 3.0)
    val b = SparseVector(3.0, 4.0, 5.0)
    (a:Vector[Double]) += (b: Vector[Double])
    assert(a === SparseVector(4.,6.,8.))
    assert((a: Vector[Double]).dot (b: Vector[Double]) === (a dot b))
    (a:Vector[Double]) *= (b: Vector[Double])
    assert(a === SparseVector(12.,24.,40.))
  }

}
