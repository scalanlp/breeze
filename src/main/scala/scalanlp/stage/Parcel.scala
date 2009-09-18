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
@SerialVersionUID(1)
case class Parcel[M,D](history : History, meta : HTMap[M], data : D)
(implicit mI: Manifest[M], mD : Manifest[D]) extends Signature {
  
  /** Applies the given stage to this parcel, returning the output of stage.process(this). */
  def ~> [InputMeta>:M, OutputMeta, OutputData]
  (stage : Stage[InputMeta, OutputMeta, D, OutputData])
  (implicit mO : Manifest[OutputMeta]) =
    stage.process(this);

  /** Creates a new parcel by applying the given function to the data in this parcel. */
  def map[O](f : D => O)(implicit mO : Manifest[O]) : Parcel[M,O] =
    Parcel(history, meta, f(data));
  
  /**
   * Creates a new Parcel that incorporates additional meta information into
   * the map.  Otherwise identical.
   */
  def withMeta[A](additionalMeta : A)(implicit mA : Manifest[A]) : Parcel[M with A, D] = {
    val newMeta = meta + additionalMeta;
    Parcel(history, newMeta, data);
  }
  
  /**
   * Creates a new Parcel with the same meta information and data but with
   * an additional piece of history newHistory.
   */
  def withHistory(stage : Stage[_,_,_,_]) : Parcel[M, D] = {
    Parcel(history + stage, meta, data);
  }

  /**
   * Returns a human readable dsecription of this parcel, useful for printing.
   */
  def description = {
    val exclusions = Set("Any","java.lang.Object","Unit","void");
    val simplified = mI.toString.split(" with ").toList.filter(s => !exclusions.contains(s)).removeDuplicates;
    
    ( Iterator.fromValues("Parcel id="+signature,
                          "  data:",
                          "    " + mD.toString,
                          "  meta:") ++
      (for (string <- simplified.elements) yield "    " + string) ++
      Iterator.fromValues("  history:",
                          History.prefixLines(history.description, "    "))
    ).mkString(System.getProperty("line.separator"));
  }
  
  /**
   * Delegates to history.signature.
   */
  override def signature =
    history.signature;
  
  override def toString() =
    history.toString;
}

/**
 * A companion object for Parcel that includes a simple static constructors.
 * 
 * @author dramage
 */
object Parcel {
  def apply[D](origin : History, data : D)(implicit mD : Manifest[D]) : Parcel[Unit,D] =
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
  def + (stage : Stage[_,_,_,_]) =
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
  case class Continuation(parent : History, stage : Stage[_,_,_,_]) extends History {
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
