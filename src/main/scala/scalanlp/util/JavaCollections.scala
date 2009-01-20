/*

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

  implicit def iScalaEnumeration[T](iterator : java.util.Enumeration[T]) : scala.Iterator[T] = {
    new scala.Iterator[T] {
      override def hasNext = iterator.hasMoreElements;
      override def next = iterator.nextElement;
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
