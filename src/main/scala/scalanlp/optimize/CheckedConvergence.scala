package scalanlp.optimize
/*
Copyright 2009 David Hall, Daniel Ramage

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at 

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. 
*/

import scalala.tensor.Vector;
import scalala.Scalala._;
import scalala.tensor._;
import scalala.tensor.operators._;
import TensorShapes._;

/**
 * A trait for optimization procedures that need to see if they're converged.
 * 
 * @author dlwh
 */
trait CheckedConvergence[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]] {
  /**
   * @return if we've converged
   */
  def checkConvergence(grad: T)
                      (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]): Boolean;
}

trait GradientNormConvergence[K,T<:Tensor1[K] with TensorSelfOp[K,T,Shape1Col]] extends CheckedConvergence[K,T] {
  val TOLERANCE = 1E-4;
  def checkConvergence(grad: T)
                      (implicit arith: Tensor1Arith[K,T,Tensor1[K],Shape1Col]): Boolean = {
    norm(grad,2) < TOLERANCE;
  }
}