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
}
