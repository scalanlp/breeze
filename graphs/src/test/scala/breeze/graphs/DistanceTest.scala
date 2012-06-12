package breeze.graphs

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before



import breeze.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.PriorityQueue;



/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class DistanceTest extends FunSuite {
  test("distance of a simple of a one-arc system is correct") {
    import Semiring.LogSpace._;
    val g = WeightedDigraphs.fromEdgeList((0,1,2.0));
    val distances = Distance.singleSourceShortestDistances(g,0);
    assert(distances(0) === 0.0);
    assert(distances(1) === 2.0);
  }

  test("cost of a single-self loop system is its closure") {
    import Semiring.LogSpace._;
    val selfLoopScore = -1.0;
    val trueCost = doubleIsLogSpace.closure(selfLoopScore)
    val g = WeightedDigraphs.fromEdgeList((0,0,selfLoopScore));
    val distances = Distance.singleSourceShortestDistances(g,0);
    assert(distances(0) === trueCost);
  }

  test("simple all pairs example") {
    import Semiring.Tropical._;
    val g = WeightedDigraphs.fromEdgeList(
      ('A,'B,10.),('B,'A,10.),
      ('A,'D,5.),('D,'A,5.),
      ('B,'C,5.),('C,'B,5.),
      ('B,'E,10.),('E,'B,10.),
      ('B,'D,5.),('D,'B,5.),
      ('D,'E,20.),('E,'D,20.));
    val distances = Distance.allPairDistances(g);
    assert(distances('A)('B) === 10.0)
    assert(distances('A)('C) === 15.0)
    assert(distances('A)('D) === 5.0)
    assert(distances('A)('E) === 20.0)
    assert(distances('B)('A) === 10.0)
    assert(distances('B)('C) === 5.0)
    assert(distances('B)('D) === 5.0)
    assert(distances('B)('E) === 10.0)
    assert(distances('C)('A) === 15.0)
    assert(distances('C)('B) === 5.0)
    assert(distances('C)('D) === 10.0)
    assert(distances('C)('E) === 15.0)
    assert(distances('D)('E) === 15.0)
    assert(distances('D)('C) === 10.0)
    assert(distances('E)('D) === 15.0)
  }


}