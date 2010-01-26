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
package scalanlp.stage;

import scala.reflect.Manifest;

import scalara.ra.Signature;

import scalanlp.collection.immutable.HTMap;

/**
 * A parcel is a paired piece of data with some meta information about
 * that data that can be passed down a pipeline through Stage instances.
 * 
 * @author dramage
 */
@serializable
@SerialVersionUID(2)
case class Parcel[D](history : History, meta : HTMap, data : D)
(implicit mD : Manifest[D]) extends Signature {
  
  /** Applies the given stage to this parcel, returning the output of stage.process(this). */
  def ~> [O](stage : Stage[D, O]) =
    stage(this);

  /**
   * Returns a human readable dsecription of this parcel, useful for printing.
   */
  def description = {
    val simplified = meta.keySet;
    
    ( Iterator("Parcel id="+signature,
               "  data:",
               "    " + mD.toString,
               "  meta:") ++
      (for (string <- simplified.iterator) yield "    " + string) ++
      Iterator("  history:",
               History.prefixLines(history.description, "    "))
    ).mkString(System.getProperty("line.separator"));
  }
  
  /**
   * Delegates to history.signature.
   */
  override def signature =
    history.signature;
  
  override def toString =
    history.toString;
}

/**
 * A companion object for Parcel that includes a simple static constructors.
 * 
 * @author dramage
 */
object Parcel {
  implicit def apply[D](data : D)(implicit mD : Manifest[D]) : Parcel[D] = {
    val stack = Thread.currentThread.getStackTrace;
    val currentClass = stack(0).getClassName;
    val currentPackage = currentClass.substring(0, currentClass.lastIndexOf("."));
    val caller = stack.dropWhile(_.getClassName.startsWith(currentPackage))(0);
    new Parcel(History.Origin(caller.toString), HTMap(), data);
  }

  def apply[D](origin : History, data : D)(implicit mD : Manifest[D]) : Parcel[D] =
    new Parcel(origin, HTMap(), data);
}


/**
 * Object describing the history of a compuation in the pipeline.
 * See concrete implementations in companion object.
 * 
 * @author dramage
 */
@serializable
trait History extends Signature {
  def + (stage : Stage[_,_]) =
    History.Continuation(this, stage);
  
  def description : String =
    toString;
}

/**
 * Companion object for History with concrete instantiations.
 * 
 * @author dramage
 */
object History {
  private val sep = System.getProperty("line.separator");
  private[stage] def prefixLines(content : String, prefix : String) =
    prefix + content.replaceAll(sep, sep+prefix);
  
  /**
   * A named origin of a computation.
   * 
   * @author dramage
   */
  case class Origin(name : String) extends History {
    override def toString =
      name;
  }

  /**
   * The continuation of a chain of computation.
   * 
   * @author dramage
   */
  case class Continuation(parent : History, stage : Stage[_,_]) extends History {
    override def toString =
      parent.toString + " ~> " + stage;
    
    override def description =
      parent.description + " ~> " + sep + "  " + stage; 
  }
  
  /**
   * A compound history with a particular name and a given set of
   * contributing histories.
   * 
   * @author dramage
   */
  case class Compound(name : String, arguments : Seq[History]) extends History {
    override def toString =
      name + "(" + arguments.mkString(", ") + ")";
    
    override def description =
      name + " " + arguments.map(_.signature).mkString(" ") +
      ( for (arg <- arguments) yield prefixLines(arg.description, "  ")
      ).mkString(sep);
  }
}
