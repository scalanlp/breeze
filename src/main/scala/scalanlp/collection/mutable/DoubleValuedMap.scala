package scalanlp.collection.mutable;
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

import scala.collection.mutable.Map;
import it.unimi.dsi.fastutil.objects.Object2DoubleOpenHashMap;
trait DoubleValuedMap[K] extends Map[K,Double] {
  override def apply(k: K): Double;
  override def default(k:K): Double = super.default(k);
}

object DoubleValuedMap {
  def apply[K]():DoubleValuedMap[K] = new  FastAny2DoubleMap[K];
  
  class FastAny2DoubleMap[K] extends DoubleValuedMap[K] {
    val underlying = new Object2DoubleOpenHashMap[K];
    def get(k: K) = if(underlying.containsKey(k)) Some(underlying.getDouble(k)) else None;
    override def apply(k:K) = if(underlying.containsKey(k)) underlying.getDouble(k) else default(k);
    def -=(k:K):this.type = { underlying.remove(k); this}
    def +=(k: (K,Double)) :this.type = { update(k._1,k._2); this }
    override def size = underlying.size;
    override def update(k:K, d:Double) = underlying.put(k,d);
    def iterator = new Iterator[(K,Double)] {
      val outer = underlying.object2DoubleEntrySet.iterator;
      def next = { 
        val n = outer.next;
        (n.getKey,n.getDoubleValue)
      }

      def hasNext = outer.hasNext;
    }
  }
}
