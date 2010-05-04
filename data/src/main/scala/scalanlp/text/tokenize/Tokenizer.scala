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
trait Tokenizer extends (String => Iterable[String]) {
  def andThen(g : Transformer) =
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
    TextSerialization.skipWhitespace(input);
    while (input.hasNext && input.head == '~') {
      TextSerialization.expect(input,"~>",true);
      TextSerialization.skipWhitespace(input);
      val next = TextSerialization.read[Transformer](input);
      TextSerialization.skipWhitespace(input);
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


  /** A tokenizer chained with a Transformer. */
  class Chain(val f : Tokenizer, val g : Transformer) extends Tokenizer {
    override def apply(txt : String) = g(f(txt));
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
        skipWhitespace(in);
        expect(in, "~>", false);
        skipWhitespace(in);
        val trn : Transformer = TextSerialization.read[Transformer](in);
        skipWhitespace(in);

        var rv = new Chain(tok, trn);

        while (in.hasNext && in.head == '~') {
          expect(in, "~>", false);
          skipWhitespace(in);
          skipWhitespace(in);
          val next = TextSerialization.read[Transformer](in);
          skipWhitespace(in);
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


//
//trait Function1Like[@specialized A,@specialized B,+This<:Function1Like[A,B,This]]
//extends (A=>B) {
//  protected def repr = this.asInstanceOf[This];
//
//  def ~>[C,G<:Function1Like[B,C,G]](g : G) =
//    new ChainedFunction[A,B,C,This,G](repr, g);
//
//  def ~>[C](g : (B=>C)) =
//    new ChainedFunction[A,B,C,This,WrappedFunction1[B,C]](repr, new WrappedFunction1(g));
//}
//
////trait Function1[@specialized A, @specialized B] extends Function1Like[A,B,Function1[A,B]];
////object Function1 extends SubtypedCompanion2[Function1[A,B]] {
////  override def continueParsing(input : TextSerialization.Input, current : Function1[A,B]) = {
////    TextSerialization.skipWhitespace(input);
////    if (input.head == '~') {
////      TextSerialization.expect(input,"~>",true);
////    }
////
////  }
////}
//
//class WrappedFunction1[@specialized A, @specialized B](val func : (A=>B))
//extends Function1Like[A,B,WrappedFunction1[A,B]] {
//  override def apply(a : A) = func(a);
//}
//
//class ChainedFunction[@specialized A, @specialized B, @specialized C,
//                      +F<:Function1Like[A,B,F], G<:Function1Like[B,C,G]]
//(val f : F, val g : G) extends Function1Like[A,C,ChainedFunction[A,B,C,F,G]] {
//  override def apply(in : A) = g(f(in));
//  override def toString = f.toString + " ~> " + g.toString;
//}
//
//object ChainedFunction {
//  implicit def textReadWritable[A,B,C,F<:Function1Like[A,B,F],G<:Function1Like[B,C,G]]
//  (cf : ChainedFunction[A,B,C,F,G])
//  (implicit rwF : TextSerialization.ReadWritable[F], rwG : TextSerialization.ReadWritable[G]) =
//  new TextSerialization.ReadWritable[ChainedFunction[A,B,C,F,G]] {
//    override def read(input : TextSerialization.Input) = {
//      val f = TextSerialization.read[F](input);
//      TextSerialization.skipWhitespace(input);
//      TextSerialization.expect(input, "~>", false);
//      TextSerialization.skipWhitespace(input);
//      val g = TextSerialization.read[G](input);
//      f ~> g;
//    }
//
//    override def write(out : TextSerialization.Output, fn : ChainedFunction[A,B,C,F,G]) {
//      TextSerialization.write(out, fn.f);
//      out.append(" ~> ");
//      TextSerialization.write(out, fn.g);
//    }
//  }
//}

///**
// * TokenizerLike extends Function1Like with chaining that keeps the function
// * signature as a Tokenizer.
// *
// * @author dramage
// */
//trait TokenizerLike[+This<:TokenizerLike[This]]
//extends Function1Like[String,Iterable[String],This] {
//
//  def ~>[G<:Function1Like[Iterable[String],Iterable[String],G]](g : G) =
//    new ChainedTokenizer[This,G](repr, g);
//
//  def ~>(g : (Iterable[String] => Iterable[String])) =
//    new ChainedTokenizer[This,WrappedFunction1[Iterable[String],Iterable[String]]](repr, new WrappedFunction1(g));
//}
//
///**
// * Abstract trait for tokenizers, which act as functions from a String
// * to an Iterable[String].  See companion object for instructions on
// * registering new subtypes outside of the current package.
// *
// * @author dramage
// */
//trait Tokenizer extends TokenizerLike[Tokenizer];
