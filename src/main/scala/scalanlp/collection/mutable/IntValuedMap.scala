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
import it.unimi.dsi.fastutil.objects.Object2IntOpenHashMap;
trait IntValuedMap[K] extends Map[K,Int] {
  override def apply(k: K): Int;
  override def default(k:K): Int = super.default(k);
}

object IntValuedMap {
  def apply[K]():IntValuedMap[K] = new  FastAny2IntMap[K];
  
  class FastAny2IntMap[K] extends IntValuedMap[K] {
    val underlying = new Object2IntOpenHashMap[K];
    def get(k: K) = if(underlying.containsKey(k)) Some(underlying.getInt(k)) else None;
    override def apply(k:K) = if(underlying.containsKey(k)) underlying.getInt(k) else default(k);
    def -=(k:K): this.type = { underlying.remove(k); this; }
    def +=( k:(K,Int)): this.type = { underlying.put(k._1,k._2); this; }
    override def size = underlying.size;
    override def update(k:K, d:Int) = underlying.put(k,d);
    def iterator = new Iterator[(K,Int)] {
      val outer = underlying.object2IntEntrySet.iterator;
      def next = { 
        val n = outer.next;
        (n.getKey,n.getIntValue)
      }

      def hasNext = outer.hasNext;
    }
  }
}
