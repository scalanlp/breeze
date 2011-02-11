package scalanlp.optimize

import scalanlp.util.Logged
import scalanlp.util.Log._
import scalala.tensor._
import scalala.Scalala._
import scalala.tensor.operators._;
import TensorShapes._;

/**
 * 
 * @author dlwh
 */

trait FirstOrderMinimizer[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col],DF<:DiffFunction[K,T]]
  extends Minimizer[T,DF] with Logged with CheckedConvergence[K,T] {

  type History;
  case class State protected[FirstOrderMinimizer](x: T, value: Double,
                                                   grad: T,
                                                   adjustedGradient: T,iter: Int,
                                                   history: History);


  def iterations(f: DF,init: T): Iterator[State];

  def minimize(f: DF, init: T):T = {
    val steps = iterations(f,init);
    val convSteps = for( cur@State(x,v,grad, adjGradient, iter,history)  <- steps) yield {
      log(INFO)("Iteration: " + iter);
      log(INFO)("Current v:" + v);
      log(INFO)("Current grad norm:" + norm(adjGradient,2));
      cur
    }

    convSteps.reduceLeft( (a,b) => b).x;
  }
}