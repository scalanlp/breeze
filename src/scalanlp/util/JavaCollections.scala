/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalanlp.util;

/**
 * Common helpful implicit function definitions for easy scala scripting.
 */
object JavaCollections {
  
  //
  // Iterators
  //
  
  implicit def iScalaIterator[T](iterator : java.util.Iterator[T]) : scala.Iterator[T] = {
    new scala.Iterator[T] {
      override def hasNext = iterator.hasNext();
      override def next = iterator.next();
    }
  }
  
  implicit def iJavaIterator[T](iterator : scala.Iterator[T]) : java.util.Iterator[T] = {
    new java.util.Iterator[T] {
      override def hasNext = iterator.hasNext;
      override def next = iterator.next;
      override def remove = throw new UnsupportedOperationException;
    }
  }
  
  implicit def iScalaIterable[T](iterable : java.lang.Iterable[T]) : scala.Iterable[T] = {
    new scala.Iterable[T] {
      override def elements = iterable.iterator();
    }
  }
  
  implicit def iJavaIterable[T](iterable : scala.Iterable[T]) : java.lang.Iterable[T] = {
    new java.lang.Iterable[T] {
      override def iterator = iterable.elements;
    }
  }
   
  //
  // Collections
  //
  
  implicit def iScalaMap[K,V](javaMap : java.util.Map[K,V]) : scala.collection.jcl.Map[K,V] = {
    new scala.collection.jcl.Map[K,V] with scala.collection.jcl.MapWrapper[K,V] {
      def underlying = javaMap;
    }
  }
  
  implicit def iJavaMap[K,V](scalaMap : scala.collection.Map[K,V]) : java.util.Map[K,V] = {
    new java.util.AbstractMap[K,V] {
      override def get(key : Object) = scalaMap.get(key);
      override def put(key : K, value : V) = scalaMap.put(key,value);
      override def entrySet = new java.util.AbstractSet[java.util.Map.Entry[K,V]] {
        override def size = scalaMap.size;
        override def iterator = new java.util.Iterator[java.util.Map.Entry[K,V]] {
          val iter = scalaMap.elements;
          override def hasNext = iter.hasNext;
          override def next = new java.util.Map.Entry[K,V] {
            val value = iter.next;
            override def getKey = value._1;
            override def getValue = value._2;
            override def setValue(o:V) = throw new UnsupportedOperationException();
            override def equals(o:Any) = {
              if (!o.isInstanceOf[java.util.Map.Entry[K,V]]) {
                false;
              } else {
                val asEntry = o.asInstanceOf[java.util.Map.Entry[K,V]];
                asEntry.getKey == getKey && asEntry.getValue == getValue;
              }
            }
            override def hashCode = value.hashCode;
          }
          override def remove = throw new UnsupportedOperationException();
        }
      }
    }
  }
  
  implicit def iScalaSeq[T](list : java.util.List[T]) : Seq[T] = {
    new Seq[T]() {
      override def length = list.size;
      override def elements = list.iterator;
      override def apply(index : Int) : T = list.get(index);
    }
  }
  
  implicit def iJavaList[E](seq : Seq[E]) : java.util.List[E] = {
    new java.util.AbstractList[E] {
      override def get(index : Int) = seq(index)
      override def size = seq.length
      override def iterator = seq.iterator;
    }
  }
  
  //
  // Java boxed types
  //
   
  implicit def iInteger[T](value : java.lang.Integer) = value.intValue()
}
