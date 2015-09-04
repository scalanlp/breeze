package breeze.stats.mcmc

import breeze.stats.distributions._
import spire.implicits.cfor
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**Tests for breeze.stats.mcmc.MetropolisHastings
 * Test for clip is currently located in "DenseVectorTest.scala"
 * @author stucchio
 * @date 3/13/14.
 */
@RunWith(classOf[JUnitRunner])
class metropolisTest extends FunSuite {

  val rand: RandBasis = RandBasis.mt0

  sealed trait State
  case object A extends State
  case object B extends State
  case object C extends State

  private val NUM_TESTS = 900000
  private val DROP_COUNT = 2

  private val l6 = math.log(6) //performance hack
  private val l2 = math.log(2)
  private val l1 = math.log(1)
  def logLikelihood(x: State) = x match {
    case A => l6
    case B => l2
    case C => l1
  }

  val proposal = rand.choose(Seq(A,B,C))

  val TOLERANCE = 0.05

  test("stupidly simple mcmc") {
    val mh = ArbitraryMetropolisHastings(logLikelihood _, (_:State) => proposal, (_:State,_:State) => 0.0, A, burnIn = 10000, dropCount=DROP_COUNT)
    var aCount: Double = 0
    var bCount: Double = 0
    var cCount: Double = 0
    cfor(0)(i => i < NUM_TESTS, i => i+1)(i => {
      mh.draw() match {
        case A => aCount += 1
        case B => bCount += 1
        case C => cCount += 1
      }
    })
    assert(math.abs(aCount / cCount - 6) < TOLERANCE)
    assert(math.abs(aCount / bCount - 3) < TOLERANCE)
    assert(math.abs(bCount / cCount - 2) < TOLERANCE)
  }

  def skewedProposal(x: State) = rand.choose(Seq(A,A,B,C).filter(_ != x) )

  def logSkewedTransitionProbability(start: State, end: State) = (start, end) match {
    case (a,b) if (a == b) => ???
    case (A,_) => math.log(0.5)
    case (_,A) => math.log(2.0/3.0)
    case (_,_) => math.log(1.0/3.0)
  }

  test("stupidly simple mcmc, anisotropic") {
    val mh = ArbitraryMetropolisHastings(logLikelihood _, skewedProposal _, logSkewedTransitionProbability _, A, burnIn = 30000, dropCount=DROP_COUNT)
    var aCount: Double = 0
    var bCount: Double = 0
    var cCount: Double = 0
    cfor(0)(i => i < NUM_TESTS, i => i+1)(i => {
      mh.draw() match {
        case A => aCount += 1
        case B => bCount += 1
        case C => cCount += 1
      }
    })
    assert(math.abs(aCount / cCount - 6) < TOLERANCE)
    assert(math.abs(aCount / bCount - 3) < TOLERANCE)
    assert(math.abs(bCount / cCount - 2) < TOLERANCE)
  }

}
