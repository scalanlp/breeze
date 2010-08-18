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
 * Mix-in trait that provides a .signature method on classes based
 * on computing a hash of toString.
 *
 * @author dramage
 */
trait Signature {
  def signature =
    HasSignature.StringHasSignature.signatureFor(this.toString);
}

object Signature {
  def apply[X:HasSignature](x : X) =
    implicitly[HasSignature[X]].signatureFor(x);
}

/**
 * A marker trait for types that support returning a simple signature
 * of their contents.
 * 
 * @author dramage
 */
trait HasSignature[-X] {
  def signatureFor(x : X) : String;
}

trait LowPriorityHasSignature {
  implicit object AnyHasSignature extends HasSignature[Any] {
    override def signatureFor(x : Any) = {
      HasSignature.StringHasSignature.signatureFor(x.toString);
    }
  }
}

object HasSignature extends LowPriorityHasSignature {
  implicit object StringHasSignature extends HasSignature[String] {
    override def signatureFor(x : String) = {
      val hex = x.toString.hashCode.toHexString;
      "0"*(8-hex.length)+hex;
    }
  }

  implicit object SignatureHasSignature extends HasSignature[Signature] {
    override def signatureFor(x : Signature) =
      x.signature
  }

  implicit object IntHasSignature extends HasSignature[Int] {
    override def signatureFor(x : Int) = x.toString;
  }
}
