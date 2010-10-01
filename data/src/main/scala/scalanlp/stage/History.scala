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
package scalanlp;
package stage;

import scalanlp.ra.Signature;

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
