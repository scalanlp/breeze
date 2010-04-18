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
package scalanlp.serialization;

/**
 * Mix-in trait for companion object to case classes to automatically
 * support StringSerialization toString and fromString.
 *
 * @author dramage
 */
trait TypedCaseCompanion1[P1,This<:AnyRef] {
  import StringSerialization._;

  /** Static constructor. */
  def apply(p1 : P1) : This;

  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def tccUnpack(t : This) : P1 = {
    val name = t.getClass.getDeclaredFields()(0).getName;
    t.getClass.getMethod(name).invoke(t).asInstanceOf[P1];
  }

  /**
   * Returns the name of the case class.  This method depends on the
   * particulars of case class encoding.
   */
  def tccName : String = {
    val name = getClass.getSimpleName;
    if (name.endsWith("$")) name.substring(0,name.length-1) else name;
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  implicit def tccReadWritable
  (implicit p1H : ReadWritable[P1])
  = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, tccName, false);
      expect(in, '(', false);
      val p1 = p1H.read(in);
      expect(in, ')', false);
      apply(p1);
    }

    override def write(out : Output, value : This) = {
      out.append(tccName);
      out.append('(');
      p1H.write(out, tccUnpack(value));
      out.append(')');
    }
  }
}


/**
 * Mix-in trait for companion object to case classes to automatically
 * support StringSerialization toString and fromString.
 *
 * @author dramage
 */
trait TypedCaseCompanion2[P1,P2,This<:AnyRef] {
  import StringSerialization._;

  /** Static constructor. */
  def apply(p1 : P1, p2 : P2) : This;

  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def tccUnpack(t : This) : (P1,P2) = {
    val n1 = t.getClass.getDeclaredFields()(1).getName;
    val p1 = t.getClass.getMethod(n1).invoke(t).asInstanceOf[P1];
    val n2 = t.getClass.getDeclaredFields()(0).getName;
    val p2 = t.getClass.getMethod(n2).invoke(t).asInstanceOf[P2];
    (p1,p2);
  }

  /**
   * Returns the name of the case class.  This method depends on the
   * particulars of case class encoding.
   */
  def tccName : String = {
    val name = getClass.getSimpleName;
    if (name.endsWith("$")) name.substring(0,name.length-1) else name;
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  implicit def tccReadWritable
  (implicit p1H : ReadWritable[P1], p2H : ReadWritable[P2])
  = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, tccName, false);
      expect(in, '(', false);
      val p1 = p1H.read(in);
      expect(in, ',', false);
      val p2 = p2H.read(in);
      expect(in, ')', false);
      apply(p1, p2);
    }

    override def write(out : Output, value : This) = {
      out.append(tccName);
      out.append('(');
      val (p1,p2) = tccUnpack(value);
      p1H.write(out, p1);
      out.append(',');
      p2H.write(out, p2);
      out.append(')');
    }
  }
}
