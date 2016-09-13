package breeze.symbolic

import breeze.linalg.DenseVector
import CanVectorize._
import org.junit.runner.RunWith
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

/**
  * These tests check that vectorizing a <code>Seq[SymbolicFunction[_]]</code> yields the same values
  * as evaluating the functions from the sequence individually and then putting them into a vector.
  */
@RunWith(classOf[JUnitRunner])
class CanVectorizeTest extends FunSuite with Checkers with Matchers {
  val complicatedFunction = Var() *:* Exponential(Var() - Const(1.0) +:+ Division(Const(3.0), Var() +:+ Const(50.0))) +:+ Var()

  test("vectorizes constants") {
    testVectorization(Const(100.0))
  }

  test("vectorizes variables") {
    testVectorization(Var())
  }

  test("vectorizes complicated formulas") {
    testVectorization(complicatedFunction)
  }

  test("vectorizes shapeless records") {
    import shapeless._
    import shapeless.record._
    import shapeless.syntax.singleton._
    val funcRecord = ("foo" ->> Const(1.0)) :: ("bar" ->> complicatedFunction) :: HNil
    val funcRecordVectorized = vectorize(Seq(funcRecord))
    compareOriginalAndVectorized(funcRecord("foo"), funcRecordVectorized("foo"))
    compareOriginalAndVectorized(funcRecord("bar"), funcRecordVectorized("bar"))
  }

  def testVectorization[SF <: SymbolicFunction[SF], SFV <: SymbolicFunction[SFV]](sf: SF)(
    implicit canCreateFunction: CanCreateFunction[Double, Double, SF],
    canVectorize: CanVectorize.Aux[SF, SFV],
    canCreateVectorizedFunction: CanCreateFunction[DenseVector[Double], DenseVector[Double], SFV]
  ): Unit = {
    val vectorizedSymbolicFunction = canVectorize.vectorize(Seq(sf))
    compareOriginalAndVectorized(sf, vectorizedSymbolicFunction)
  }

  def compareOriginalAndVectorized[SFV <: SymbolicFunction[SFV], SF <: SymbolicFunction[SF]](
    sf: SF,
    vectorizedSymbolicFunction: SFV
  )(
    implicit canCreateFunction: CanCreateFunction[Double, Double, SF],
    canCreateVectorizedFunction: CanCreateFunction[DenseVector[Double], DenseVector[Double], SFV]
  ): Unit = {
    val func = canCreateFunction.createFunction(sf)
    // Helpful for debugging
    // println(s"F  = $sf\nF' = $vectorizedSymbolicFunction")
    val vectorizedFunc = canCreateVectorizedFunction.createFunction(vectorizedSymbolicFunction)
    (-20.0 to 20.0 by 0.5).foreach { x =>
      val value = func(x)
      val vector = vectorizedFunc(DenseVector(x))
      // Helpful for debugging:
      // println(s"For x = $x comparing $diffLeft <=> ${Vectorization * distance} <=> $diffRight")
      (vector(0)) should be(value)
    }
  }
}
