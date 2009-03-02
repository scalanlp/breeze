package scalanlp.optimize;

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

/**
* Anything that can minimize a Diff Function
*
* @author dlwh
*/
trait Minimizer[T<:Seq[Double],F<:Function[T,Double]] {
  def minimize(f: F, initial: T):T = minimize(f,initial,1E-4)
  def minimize(f: F, initial: T, tol: Double): T = minimize(f,initial, tol, -1)
  def minimize(f: F, initial: T, tol: Double, maxIter: Int): T;
}
