package scalanlp.concurrent
/*
 Copyright 2010 David Hall, Daniel Ramage

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
 * A more Scala-like ThreadLocal. Takes a default value that is created per thread.
 *
 * @author dlwh
 */
class ThreadLocal[T](default: =>T) extends java.lang.ThreadLocal[T] with Function0[T] {
  override protected def initialValue: T = default;
  def apply() = get();
}
