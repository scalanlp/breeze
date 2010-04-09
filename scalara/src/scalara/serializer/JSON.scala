/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package scalara.serializer


object JSON {
  abstract class JSONSerializable {
    def toJSON : Iterator[String];
  }

  object JSONSerializable {
    def apply(json : =>String) = new JSONSerializable {
      override def toJSON = Iterator(json);
    }
  }

  abstract class JSONSerializableDepth2[A<%JSONSerializable] extends JSONSerializable;

  object JSONSerializableDepth2 {
    def apply[A<%JSONSerializable](json : =>String) = new JSONSerializableDepth2[A] {
      override def toJSON = Iterator(json);
    }
  }


  abstract class JSONSerializableDepth3[B,A<%JSONSerializableDepth2[B]] extends JSONSerializableDepth2[A];

  implicit def iToJSONSerializable(str : String) =
    if (str == null) "null" else "\"" + escapeJSON(str) + "\"";

  implicit def iToJSONSerializable(v : Double) = v.toString;
  implicit def iToJSONSerializable(v : Float) = v.toString;
  implicit def iToJSONSerializable(v : Int) = v.toString;
  implicit def iToJSONSerializable(v : Long) = v.toString;
  implicit def iToJSONSerializable(v : Short) = v.toString;
  implicit def iToJSONSerializable(v : Char) = v.toString;
  implicit def iToJSONSerializable(v : Boolean) = v.toString;

  implicit def iToJSONSerializable(v : Array[Double]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Float]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iIntArrayToJSONSerializable(v : Array[Int]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Long]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Short]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Char]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable(v : Array[Boolean]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.mkString(", ")+"]"; }
  implicit def iToJSONSerializable1[A<%JSONSerializable](v : Array[A]) = JSONSerializable {
    if (v == null) "null" else "[ " + v.iterator.map(_.toJSON).mkString(", ")+"]"; }
  implicit def iToJSONSerializable2[B,A<%JSONSerializable](v : Array[A]) = JSONSerializableDepth2[A] {
    if (v == null) "null" else "[ " + v.iterator.map(_.toJSON).mkString(", ")+"]"; }

  @inline final private def escapeChar(c : Char) : String = c match {
    case '"'  => "\\\"";
    case '\\' => "\\\\";
    case '/'  => "\\/";
    case '\b' => "\\b";
    case '\f' => "\\f";
    case '\n' => "\\n";
    case '\r' => "\\r";
    case '\t' => "\\t";
    case c if ((c >= '\u0000' && c <= '\u001F') || (c >= '\u007F' && c <= '\u009F') || (c >= '\u2000' && c <= '\u20FF')) =>
      { val hex = c.toHexString.toUpperCase; "\\u"+("0"*(4-hex.length))+hex; }
    case _ => c.toString;
  }

  def escapeJSON(str : String) : String =
    str.flatMap(escapeChar)


  def manifestOf[V](implicit m : Manifest[V]) = m;

  val x = Array(Array(1,2),Array(3),Array(4,5,6));
  // iToJSONSerializable(x)(manifestOf[Array[Int]], iIntArrayToJSONSerializable).toJSON;
  // println(x.toJSON.mkString("\n"));
}
