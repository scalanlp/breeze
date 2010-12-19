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

import scalala.library.Library._
import scalala.generic.math.CanNorm
;

/**
 * A trait for gradient-based optimization procedures that need to see if they're converged.
 * 
 * @author dlwh
 */
trait CheckedConvergence[T] {
  /**
   * @return if we've converged
   */
  def checkConvergence(v: Double, grad: T): Boolean;
}

trait GradientNormConvergence[T] extends CheckedConvergence[T] {
  protected implicit val canNorm: CanNorm[T]
  val TOLERANCE = 1E-6;
  def checkConvergence(v: Double, grad: T): Boolean = {
    val vv = if(v.abs < 1E-3) 1E-3 else v.abs;
    norm(grad,2)/vv < TOLERANCE;
  }
}