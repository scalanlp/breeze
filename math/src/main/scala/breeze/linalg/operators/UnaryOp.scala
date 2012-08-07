package breeze.linalg.operators
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import collection.mutable.HashMap
import breeze.generic.{MethodImpl, Multimethod}

/**
 * This is the capability trait for operations of the form -a.
 * These traits are usually implemented in (a supertype of) the companion object of
 * one of the operands.
 * @tparam A
 * @tparam B
 * @tparam Op
 */
trait UnaryOp[A, Op <: OpType, +R] extends MethodImpl[A, R] {
  def apply(a: A): R
}

object UnaryOp {
  type Bind[Op <:OpType] = { type Sig[A, R] = UnaryOp[A, Op, R]}
}

/**
 * This is a special kind of UnaryOp that supports registration
 * of specialized implementations for a given operation.
 * @author dlwh
 */
trait UnaryRegistry[A <: AnyRef, Op <: OpType, R] extends UnaryOp[A, Op, R] with Multimethod[UnaryOp.Bind[Op]#Sig, A, R] {
}

