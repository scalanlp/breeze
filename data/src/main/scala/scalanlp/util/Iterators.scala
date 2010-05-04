package scalanlp.util;

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
 * Utilities and implicits for iterators. Nothing major.
 *
 * @author dramage
 */
object Iterators {

  def fromProducer[E](prod: =>Option[E]):Iterator[E] = Iterator.continually(prod).takeWhile(None !=).map(_.get);

  /**
   * Procedural iterator creation with an explicit callback.
   * 
   * TODO: this could be done smarter with synchronized blocks
   * and waiting.
   */
  def iterator[E](func : ((E=>Unit)=>Unit)) : Iterator[E] = {
    import scala.concurrent.ops._;
    
    val queue = new java.util.concurrent.LinkedBlockingQueue[E](1);
    var running = true;
    var thrown : Throwable = null;
    
    def isRunning = running;
    def isPending = queue.size >= 1;
    
    spawn {
      try {
        func(queue.put)
      } catch {
        case x : Throwable => thrown = x;
      };
      running = false;
    }
    
    return new Iterator[E] {
      def waitIfNecessary() = {
        val method = classOf[Thread].getMethod("yield");
        while (isRunning && !isPending) {
          method.invoke();
        }
        if (thrown != null) {
          throw new RuntimeException(thrown);
        }
      }
      
      override def hasNext = {
        waitIfNecessary();
        isPending;
      }
      
      override def next = {
        waitIfNecessary();
        if (!isPending) {
          throw new RuntimeException("Called next on empty iterator");
        }
        queue.take;
      }
    }
  }
  
}
