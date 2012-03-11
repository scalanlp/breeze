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


import scala.collection.mutable.WeakHashMap;
import scala.collection.generic._;
import scala.collection._;
import scala.collection.Traversable;
import scala.collection.TraversableLike;
import java.lang.ref.WeakReference
import java.io.{ObjectInputStream, ObjectOutputStream}
;

/**
 * Class that mimics Java's string interner, but for anything.
 * Threadsafe.
 *
 * @author dlwh
 */
class Interner[T] extends (T=>T) with Serializable {
  override def apply(t :T) = intern(t);

  def intern(t : T):T = synchronized {
    inner.getOrElseUpdate(t,new WeakReference[T](t)).get;
  }

  def clear() = inner.clear();
  def size = inner.size;

  def internAll[C<:TraversableLike[T,C] with Traversable[T],That](c : C)(implicit bf:CanBuildFrom[T,That,C]) = c map apply
  def internAll(c : List[T]) = c map apply
  def internAll(c : Array[T]) = c map apply
  def internAll(c : Set[T]) = c map apply

  def internKeys[V](c: scala.collection.Map[T,V]) = {
    Map[T,V]() ++ c.map{ case (k,v) => (intern(k),v)}
  }

  def internValues[K](c: scala.collection.Map[K,T]) = {
    Map[K,T]() ++ c.map{ case (k,v) => (k,intern(v))}
  }

  @transient private var inner = new WeakHashMap[T,WeakReference[T]];

  @throws(classOf[java.io.IOException])
  private def writeObject(oos: ObjectOutputStream ) {
    oos.defaultWriteObject();
  }

  @throws(classOf[java.io.IOException])
  @throws(classOf[ClassNotFoundException])
  private def readObject(ois: ObjectInputStream) {
    ois.defaultReadObject();
    inner = new WeakHashMap[T,WeakReference[T]]
  }

}

object Interner {
  private val typedInterners = new scala.collection.mutable.HashMap[Class[_],Interner[_]] {
    override def default(c: Class[_]) = getOrElseUpdate(c,new Interner[Any]);
  }
  
  def apply[T](implicit m: scala.reflect.Manifest[T]) = forClass[T](m.erasure.asInstanceOf[Class[T]]);

  def forClass[T](c: Class[T]) = typedInterners(c).asInstanceOf[Interner[T]];
}
