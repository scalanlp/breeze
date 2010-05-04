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
package scalanlp.text.transform;

import scalanlp.serialization.{TextSerialization,SubtypedCompanion};

/**
 * A generic (loadable) transformation of a tokenized input text.
 *
 * @author dramage
 */
trait Transformer extends (Iterable[String] => Iterable[String]) {
  override def toString = TextSerialization.toString(this);
}

object Transformer extends SubtypedCompanion[Transformer] {
  prepare();
  register[TermMinLengthFilter];

  implicit def apply(f : Iterable[String] => Iterable[String]) : Transformer = {
    f match {
      case ft : Transformer => ft;
      case _ => new Impl(f, f.toString);
    }
  }

  class Impl(val f : Iterable[String] => Iterable[String], val name : String) extends Transformer {
    override def apply(tokens : Iterable[String]) = f(tokens);
    override def toString = name;
    override def equals(other : Any) = other match {
      case that : Impl => this.f == that.f;
      case _ => false;
    }
  }
}
