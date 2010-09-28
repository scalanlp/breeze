package scalanlp.math

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.prop._;
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.PriorityQueue;

import org.scalacheck._;

trait SemiringAxioms[W] extends FunSuite with Checkers {
  def makeRing: Semiring[W];
  implicit def arb: Arbitrary[W];
  
  val ring = makeRing;
  import ring._;

  def traceEq(w1: W, w2: W) = if(closeTo(w1,w2)) true else {
    throw new Exception("No EQ! 1 :" + w1 + "\n 2: " + w2);
  }

  test("zero is the additive identity") {
    check( (w: W) => traceEq(plus(zero,w),w));
    check( (w: W) => traceEq(plus(w,zero),w));
  }
  
  test("addition commutes") {
    check( (a: W, b: W) => traceEq(plus(a,b),plus(b,a)));
  }
  
  test("addition associates") {
    check( (a: W, b: W, c: W) => traceEq(plus(plus(a,b),c),plus(a,plus(b,c))));
  }
  
  test("multiplication associates") {
    check( (a: W, b: W, c: W) => traceEq(times(times(a,b),c),times(a,times(b,c))));
  }
  
  test("multiplication has identity 1") {
    check( (w: W) => traceEq(times(one,w),w));
    check( (w: W) => traceEq(times(w,one),w));
  }
  
  test("multiplication has annihilator 0") {
    check( (w: W) => traceEq(times(zero,w),zero));
    check( (w: W) => traceEq(times(w,zero),zero));
  }
  
  test("multiplication distributes over addition") {
    check( (a: W, b: W, c: W) => traceEq(times(a,plus(b,c)),plus(times(a,b),times(a,c))));
    check( (a: W, b: W, c: W) => traceEq(times(plus(b,c),a),plus(times(b,a),times(c,a))));
  }
  
  test("zero* == 1") {
    assert( closeTo(closure(zero),one));
  }
  
}

@RunWith(classOf[JUnitRunner])
class BooleanSemiringTest extends SemiringAxioms[Boolean] {
  def arb = Arbitrary.arbBool;
  def makeRing = Semiring.booleanSemiring;   
}

@RunWith(classOf[JUnitRunner])
class ProbabilitySemiringTest extends SemiringAxioms[Double] {
  def arb = Arbitrary.arbDouble;
  def makeRing = Semiring.Probability.semiring;   
}

@RunWith(classOf[JUnitRunner])
class ViterbiSemiringTest extends SemiringAxioms[Double] {
  def arb = Arbitrary.arbDouble;
  def makeRing = Semiring.Viterbi.doubleIsViterbi;
}


@RunWith(classOf[JUnitRunner])
class TropicalSemiringTest extends SemiringAxioms[Double] {
  def arb = Arbitrary.arbDouble;
  def makeRing = Semiring.Tropical.doubleIsTropical;
}

@RunWith(classOf[JUnitRunner])
class LogSpaceSemiringTest extends SemiringAxioms[Double] {
  def arb = Arbitrary.arbDouble;
  def makeRing = Semiring.LogSpace.doubleIsLogSpace;
}
