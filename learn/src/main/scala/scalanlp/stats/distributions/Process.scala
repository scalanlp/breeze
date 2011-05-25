package scalanlp.stats.distributions

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
 * A Rand that changes based on previous draws.
 * 
 * @author dlwh
 */
trait Process[T] extends Rand[T] { process =>
  /** Force the "next" draw to be x, and return a new process. */
  def observe(x: T): Process[T];
  
  /** Draw a sample and the next step of the process along with it.*/
  def step(): (T,Process[T]) = {
    val x = get;
    (x,observe(x));
  }

  /** Returns an Iterator that automatically moves the Process along as 
    next is called */
  def steps:Iterator[T] = new Iterator[T] {
    private var current = process;
    def hasNext = true;
    def next = {
      val (x,nextP) = current.step();
      current = nextP;
      x;
    }
  }
}
