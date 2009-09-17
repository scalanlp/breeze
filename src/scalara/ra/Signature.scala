/*
 * Distributed as part of ScalaRA, a scientific research tool.
 * 
 * Copyright (C) 2007 Daniel Ramage
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
package scalara.ra

/**
 * A mix-in trait that provides a short signature of an object based on
 * the hash of its toString.  This method should only be mixed in to
 * immutable classes with a reasonable toString defined.
 * 
 * @author dramage
 */
trait Signature {
  def signature : String = {
    val hex = this.toString.hashCode.toHexString;
    "0"*(8-hex.length)+hex;
  }
}
