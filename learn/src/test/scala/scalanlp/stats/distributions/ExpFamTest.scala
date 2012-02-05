package scalanlp.stats.distributions

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Prop, Arbitrary}

/**
 * 
 * @author dlwh
 */
trait ExpFamTest[D<:Measure[T] with Rand[T],T] extends FunSuite with Checkers {
  val expFam: ExponentialFamily[D,T]
  import expFam._

  implicit def arbParameter: Arbitrary[expFam.Parameter]

  def paramsClose(p: Parameter, b: Parameter):Boolean

  test("MLE is consistent") {
    check(Prop.forAll { (p: expFam.Parameter) =>
      val dist: D = expFam.distribution(p)
      val suffstat = dist.sample(6000).map(sufficientStatisticFor _ ).reduce(_ + _)
      val mle = expFam.mle(suffstat)
      if(!paramsClose(mle, p)) {
        println("Got " + mle + " expected " + p)
        false
      } else {
        true
      }
    } )
  }
}