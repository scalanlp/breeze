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

package scalanlp.ra

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
