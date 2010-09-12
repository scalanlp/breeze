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

import com.thoughtworks.paranamer.BytecodeReadingParanamer;

import scala.reflect.ClassManifest;

import scalanlp.serialization.TextSerialization;

/**
 * <p>Base trait for automatic command line processing of objects that
 *  provide an apply method and inherent from the appropriate numbered
 *  abstract subclass of this trait.<p>
 *
 * <p> Usage: create an Object with an apply method taking some number
 *  of arguments N.  Then extends ArgumentMainN[...] providing the types
 *  of the arguments that apply() expects.  Each of argument's type must have
 *  an implicit TextSerialization.Readable[_] so that the corresponding
 *  command line argument can be de-marshalled from a String.  Default
 *  values can be provided.</p>
 *
 * Example usage:
 *
 * <code>
 * object MyObject extends ArgumentMain2[String,Int] {
 *   def apply(message : String, count : Int = 3) {
 *     println("Message: "+message);
 *     println("Count:   "+count);
 *   }
 * }
 * </code>
 *
 * <p>Then, from the command line:</p>
 *
 * <code>
 * scala mypackage.MyObject hi 7
 *   Message: hi
 *   Count:   7
 *
 * scala mypackage.MyObject --count 1 --message what?
 *   Message: what?
 *   Count:   1
 *
 * scala mypackage.MyObject "two words default count"
 *   Message: two words default count
 *   Count:   3
 * </code>
 *
 * @author dramage
 */
trait ArgumentMain {
  /** Subclasses provide the class erasure for all arguments to apply() */
  val erasures : List[Class[_]];

  /** Subclasses provide the readable instance for all arguments to apply() */
  val readables : List[TextSerialization.Readable[_]];

  private val paranamer = new com.thoughtworks.paranamer.BytecodeReadingParanamer();

  /** CustomStringReadWritable accepts unescaped string literals. */
  private val CustomStringReadable : TextSerialization.Readable[String] =
  new TextSerialization.Readable[String] {
    override def read(in : TextSerialization.Input) = {
      if (in.peek != '"') in.readRemaining;
      else TextSerialization.stringReadWritable.read(in);
    }
  }

  /**
   * Gets the readable for the given type, using some of our own command-line
   * specific getters.
   */
  protected def getReadable[X:ClassManifest:TextSerialization.ReadWritable] :
  TextSerialization.Readable[X] = {
    if (implicitly[ClassManifest[X]].erasure == classOf[String]) {
      CustomStringReadable.asInstanceOf[TextSerialization.Readable[X]];
    } else {
      implicitly[TextSerialization.ReadWritable[X]];
    }
  }

  /**
   * Gets the erasure for the given type.
   */
  protected def getErasure[X:ClassManifest:TextSerialization.ReadWritable] :
  Class[_] = implicitly[ClassManifest[X]].erasure.asInstanceOf[Class[_]];

  // protected val instance = this.getClass.getField("MODULE$").get(null);

  /** Gets the default value for the given position */
  private def getDefault(pos : Int) : Option[Object] = {
    try {
      Some(this.getClass.getMethod("apply$default$"+(pos+1)).invoke(this));
    } catch {
      case _ => return None;
    }
  }

  /**
   * Default main method: parses arguments honoring default values,
   * switches (based on the argument name), and positional arguments.
   */
  def main(args : Array[String]) {
    val method = try {
      this.getClass.getMethod("apply", erasures :_*);
    } catch {
      case ex : Throwable =>
        throw new ArgumentException("No apply method defined with types: "+erasures.mkString(" "), ex);
    }

    // parameter names
    val names : Array[String] =
      paranamer.lookupParameterNames(method);

    // values for each parameter
    val processed : Array[Option[Object]] =
      Array.tabulate(names.length)(i => None);

    // process flag arguments
    var argI = 0;
    while (argI < args.length) {
      val arg = args(argI);
      if (arg.startsWith("--") && names.contains(arg.substring(2))) {
        if (argI + 1 == args.length) {
          throw new ArgumentException("Not enough arguments for switch "+arg);
        }
        
        val value = args(argI+1);
        val paramI = names.findIndexOf(_ == arg.substring(2));

        if (processed(paramI).isDefined) {
          throw new ArgumentException("Value for "+arg+"already defined");
        }

        processed(paramI) = Some(TextSerialization.fromString(value)(readables(paramI)).asInstanceOf[Object]);

        args(argI) = null;
        args(argI+1) = null;
        argI += 2;
      } else {
        argI += 1;
      }
    }

    // process remaining by lining up positional arguments
    if (args.filter(_ != null).length > processed.filter(!_.isDefined).length) {
      throw new ArgumentException("Too many arguments");
    }
    for ((arg,paramI) <- args.filter(_ != null) zip
                         ((processed.zipWithIndex).filter(!_._1.isDefined).map(_._2))) {
      processed(paramI) = Some(TextSerialization.fromString(arg)(readables(paramI)).asInstanceOf[Object]);
    }

    // get default arguments for remaining
    for (paramI <- processed.zipWithIndex.filter(!_._1.isDefined).map(_._2)) {
      processed(paramI) = getDefault(paramI);
    }

    // make sure no nulls remain
    if (processed.filter(!_.isDefined).length > 0) {
      throw new ArgumentException("No value for all arguments");
    }

    method.invoke(this, processed.map(_.get).toArray.asInstanceOf[Array[Object]] :_*);
  }
}

/**
 * Command line support for objects that provide a 1-arg apply method.
 *
 * @author dramage
 */
abstract class ArgumentMain0 extends ArgumentMain {
  def apply();

  override val erasures = List();
  override val readables = List();
}


/**
 * Command line support for objects that provide a 1-arg apply method.
 *
 * @author dramage
 */
abstract class ArgumentMain1
[A1:ClassManifest:TextSerialization.ReadWritable]
extends ArgumentMain {
  def apply(a1 : A1);

  override val erasures =
    List(getErasure[A1]);
  override val readables =
    List(getReadable[A1]);
}

/**
 * Command line support for objects that provide a 2-arg apply method.
 *
 * @author dramage
 */
abstract class ArgumentMain2
[A1:ClassManifest:TextSerialization.ReadWritable,
 A2:ClassManifest:TextSerialization.ReadWritable]
extends ArgumentMain {
  override val erasures =
    List(getErasure[A1], getErasure[A2]);
  override val readables =
    List(getReadable[A1], getReadable[A2]);
}

/**
 * Command line support for objects that provide a 3-arg apply method.
 *
 * @author dramage
 */
abstract class ArgumentMain3
[A1:ClassManifest:TextSerialization.ReadWritable,
 A2:ClassManifest:TextSerialization.ReadWritable,
 A3:ClassManifest:TextSerialization.ReadWritable]
extends ArgumentMain {
  override val erasures =
    List(getErasure[A1], getErasure[A2], getErasure[A3]);
  override val readables =
    List(getReadable[A1], getReadable[A2], getReadable[A3]);
}

/**
 * Command line support for objects that provide a 4-arg apply method.
 *
 * @author dramage
 */
abstract class Main4
[A1:ClassManifest:TextSerialization.ReadWritable,
 A2:ClassManifest:TextSerialization.ReadWritable,
 A3:ClassManifest:TextSerialization.ReadWritable,
 A4:ClassManifest:TextSerialization.ReadWritable]
extends ArgumentMain {
  override val erasures =
    List(getErasure[A1], getErasure[A2], getErasure[A3], getErasure[A4]);
  override val readables =
    List(getReadable[A1], getReadable[A2], getReadable[A3], getReadable[A4]);
}

/**
 * Exception thrown during argument parsing.
 *
 * @author dramage
 */
class ArgumentException(msg : String, cause : Throwable)
extends RuntimeException(msg, cause) {
  def this(msg : String) = this(msg, null);
}
