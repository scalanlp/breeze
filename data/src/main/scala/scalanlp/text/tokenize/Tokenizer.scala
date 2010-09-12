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
package scalanlp.text.tokenize;

import scalanlp.text.transform.Transformer;

import scalanlp.serialization.SubtypedCompanion;
import scalanlp.serialization.TextSerialization;


/**
 * Abstract trait for tokenizers, which act as functions from a String
 * to an Iterable[String].  See companion object for instructions on
 * registering new subtypes outside of the current package.
 *
 * @author dramage
 */
@serializable @SerialVersionUID(1)
trait Tokenizer extends (String => Iterable[String]) {
  def andThen(g : Transformer) : Tokenizer =
    this ~> g;

  def ~> (g : Transformer) =
    new Tokenizer.Chain(this, g);

  override def toString = TextSerialization.toString(this);
}

/**
 * Companion object for Tokenizer that supports automatic TextSerialization
 * of Tokenizer and its subtypes.  Tokenizers not in scalanlp.text.tokenizers
 * need to call <code>Tokenizer.register[CustomTokenizer]("CustomTokenizer")</code>
 * in order for toString and fromString on Tokenizers to recognize the new type.
 *
 * @author dramage
 */
object Tokenizer extends SubtypedCompanion[Tokenizer] {
  prepare();
  register[RegexSplitTokenizer];
  register[RegexSearchTokenizer];
  register[WhitespaceTokenizer];
  register[PTBTokenizer];
  register[Chain]("Tokenizer.Chain");
  SimpleEnglishTokenizer;

  implicit def apply(f : String => Iterable[String]) : Tokenizer = {
    f match {
      case ft : Tokenizer => ft;
      case _ => new Impl(f, f.toString);
    }
  }

  /** Load tokenizer chains. */
  override def continueParsing(input : TextSerialization.Input, current : Tokenizer) : Tokenizer = {
    var rv = current;
    input.skipWhitespace;
    while (input.peek == '~') {
      input.expect("~>");
      input.skipWhitespace;
      val next = TextSerialization.read[Transformer](input);
      input.skipWhitespace;
      rv = rv ~> next;
    }
    rv;
  }

  /** Standard implementation wrapping an underlying function of String => Iterable[String]. */
  class Impl(val f : String => Iterable[String], val name : String) extends Tokenizer {
    override def apply(txt : String) = f(txt);
    override def toString = name;
    override def equals(other : Any) = other match {
      case that : Impl => this.f == that.f;
      case _ => false;
    }
    override def hashCode = f.hashCode;
  }


  /**
   * A tokenizer chained with a Transformer. If the given tokenizer is
   * not itself already a transformer, then this method will create a
   * view of the output of the tokenizer before applying transformation
   * to prevent proliferating itnermediate results.
   * 
   * @author dramage
   */
  class Chain(val f : Tokenizer, val g : Transformer) extends Tokenizer {
    protected val tokenize : (String => Iterable[String]) =
      if (f.isInstanceOf[Transformer]) f else f.andThen((i : Iterable[String]) => i.view);

    override def apply(txt : String) = g(tokenize(txt));
    override def toString = f.toString + " ~> " + g.toString;

    override def equals(other : Any) = other match {
      case that : Chain => this.f == that.f && this.g == that.g;
      case _ => false;
    }
    override def hashCode = f.hashCode * 37 + g.hashCode;
  }

  object Chain {
    import TextSerialization._;

    /**
     * Constructs a ReadWritable for the primary type T.
     */
    implicit val readWritable : TextSerialization.ReadWritable[Chain] =
    new TextSerialization.ReadWritable[Chain] {
      override def read(in : Input) : Chain = {
        val tok : Tokenizer = TextSerialization.read[Tokenizer](in);
        in.skipWhitespace;
        in.expect( "~>");
        in.skipWhitespace;
        val trn : Transformer = TextSerialization.read[Transformer](in);
        in.skipWhitespace;

        var rv = new Chain(tok, trn);

        while (in.peek == '~') {
          in.expect("~>");
          in.skipWhitespace;
          val next = TextSerialization.read[Transformer](in);
          in.skipWhitespace;
          rv = new Chain(rv, next);
        }

        rv;
      }

      override def write(out : Output, value : Chain) = {
        out.append(value.toString);
      }
    }
  }
}
