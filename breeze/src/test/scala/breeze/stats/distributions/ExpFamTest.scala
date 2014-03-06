package breeze.stats.distributions

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.{Prop, Arbitrary}
import scala.reflect.ClassTag

/**
 * 
 * @author dlwh
 */
trait ExpFamTest[D<:Density[T] with Rand[T],T] extends FunSuite with Checkers {
  val expFam: ExponentialFamily[D,T]
  import expFam._

  implicit def arbParameter: Arbitrary[expFam.Parameter]

  def paramsClose(p: Parameter, b: Parameter):Boolean


  test("MLE is consistent") {
    check(Prop.forAll { (p: expFam.Parameter) =>
      try {
        val dist = expFam.distribution(p)
        val suffstat = dist.sample(10000).map(sufficientStatisticFor).reduce(_ + _)
        val mle = expFam.mle(suffstat)
        if(!paramsClose(mle, p)) {
          println("Got " + mle + " expected " + p)
          false
        } else {
          true
        }
      } catch {
        case ex: Exception =>  ex.printStackTrace(); throw ex
      }
    } )
  }

  test("Rescale doesn't affect MLE") {
    check(Prop.forAll { (p: expFam.Parameter) =>
      val dist: D = expFam.distribution(p)
      val suffstat = dist.sample(100).map(sufficientStatisticFor).reduce(_ + _)
      val mle = expFam.mle(suffstat)
      val mle2 = expFam.mle(suffstat * .5)
      if(!paramsClose(mle, mle2)) {
        println("Got " + mle2 + " expected " + mle)
        false
      } else {
        true
      }
    } )

  }
}