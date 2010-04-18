/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalanlp.serialization

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
  def unpack(t : This) : P1 = {
    val name = t.getClass.getDeclaredFields()(0).getName;
    t.getClass.getMethod(name).invoke(t).asInstanceOf[P1];
  }

  /**
   * Returns the name of the case class.  This method depends on the
   * particulars of case class encoding.
   */
  def name : String = {
    val name = getClass.getSimpleName;
    if (name.endsWith("$")) name.substring(0,name.length-1) else name;
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  implicit def typedCaseCompanion1ReadWritable
  (implicit p1H : ReadWritable[P1])
  = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      val p1 = p1H.read(in);
      expect(in, ')', false);
      apply(p1);
    }

    override def write(out : Output, value : This) = {
      out.append(name);
      out.append('(');
      p1H.write(out, unpack(value));
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
  def unpack(t : This) : (P1,P2) = {
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
  def name : String = {
    val name = getClass.getSimpleName;
    if (name.endsWith("$")) name.substring(0,name.length-1) else name;
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  implicit def typedCaseCompanion2ReadWritable
  (implicit p1H : ReadWritable[P1], p2H : ReadWritable[P2])
  = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      val p1 = p1H.read(in);
      expect(in, ',', false);
      val p2 = p2H.read(in);
      expect(in, ')', false);
      apply(p1, p2);
    }

    override def write(out : Output, value : This) = {
      out.append(name);
      out.append('(');
      val (p1,p2) = unpack(value);
      p1H.write(out, p1);
      out.append(',');
      p2H.write(out, p2);
      out.append(')');
    }
  }
}


///**
// * Mix-in trait for companion object to case classes to automatically
// * support StringSerialization toString and fromString.
// *
// * @author dramage
// */
//trait TypedCaseCompanion[T<:Product,This<:AnyRef] {
//  import StringSerialization._;
//
//  protected def getApplyMethod(args : Int) =
//    getClass.getMethods.filter(method =>
//      method.getName == "apply" && method.getParameterTypes.length == args).head;
//
//  /** Static constructor. */
//  def construct(t : T) : This = t match {
//    case (a,b,c,d) => getApplyMethod(4).invoke(null,a,b,c,d).asInstanceOf[This];
//    case (a,b,c) => getApplyMethod(3).invoke(null,a,b,c).asInstanceOf[This];
//    case (a,b) => getApplyMethod(2).invoke(null,a,b).asInstanceOf[This];
//    case Some(v) => getApplyMethod(1).invoke(null,v).asInstanceOf[This];
//    case None => getApplyMethod(0).invoke(null).asInstanceOf[This];
//    case _ => throw new IllegalArgumentException("Unsupported argument "+t);
//  }
//
//  /**
//   * Returns the arguments given to the apply() static constructor.  This
//   * method depends on the particulars of case class encoding.
//   */
//  def unpack(t : This) : P1 = {
//    val name = t.getClass.getDeclaredFields()(0).getName;
//    t.getClass.getMethod(name).invoke(t).asInstanceOf[P1];
//  }
//
//  /**
//   * Returns the name of the case class.  This method depends on the
//   * particulars of case class encoding.
//   */
//  def name : String = {
//    val name = getClass.getSimpleName;
//    if (name.endsWith("$")) name.substring(0,name.length-1) else name;
//  }
//
//  /**
//   * Constructs a ReadWritable for the primary type T.
//   */
//  implicit def typedCaseCompanionReadWritable
//  (implicit p1H : ReadWritable[P1])
//  = new ReadWritable[This] {
//    override def read(in : Input) = {
//      expect(in, name, false);
//      expect(in, '(', false);
//      val p1 = p1H.read(in);
//      expect(in, ')', false);
//      apply(p1);
//    }
//
//    override def write(out : Output, value : This) = {
//      out.append(name);
//      out.append('(');
//      p1H.write(out, unpack(value));
//      out.append(')');
//    }
//  }
//}



case class MyCaseClass1(b : String);
object MyCaseClass1 extends TypedCaseCompanion1[String,MyCaseClass1];

case class MyCaseClass2(a : Int, b : String)
object MyCaseClass2 extends TypedCaseCompanion2[Int,String,MyCaseClass2];

case class MyCompoundCaseClass1(a : (Int,Double));
object MyCompoundCaseClass1 extends TypedCaseCompanion1[(Int,Double),MyCompoundCaseClass1];

case class MyCompoundCaseClass2(a : Int, b : MyCaseClass2)
object MyCompoundCaseClass2 extends TypedCaseCompanion2[Int,MyCaseClass2,MyCompoundCaseClass2]

case class MyCompoundCaseClass3(a : List[Double]);
object MyCompoundCaseClass3 extends TypedCaseCompanion1[List[Double],MyCompoundCaseClass3];

object Test {
  def loop[T:StringSerialization.ReadWritable](value : T) = {
    println(value);
    val string = StringSerialization.toString(value);
    println(string);
    val parsed = StringSerialization.fromString[T](string);
    println(parsed);
    println(parsed == value);
    parsed == value;
  }

  def main(args : Array[String]) {
    loop(MyCaseClass1("hi"));
    loop(MyCaseClass2(1,"hi"));
    loop(MyCompoundCaseClass1((1,2.0)))(MyCompoundCaseClass1.typedCaseCompanion1ReadWritable);
    loop(MyCompoundCaseClass2(1,MyCaseClass2(2,"yo")));
    loop(MyCompoundCaseClass3(List(1.0,2.0)))(MyCompoundCaseClass3.typedCaseCompanion1ReadWritable);
  }
}
