//package scalanlp.serialization
//
//import scala.collection.mutable.HashMap;
//import scala.reflect.ClassManifest;
//
//import TextSerialization._;
//
//class Function1Companion[A,B] {
//
//  type This = (A => B);
//
//  /** Registry of known sub-types. */
//  protected val registry = HashMap[String, (ClassManifest[_],ReadWritable[_])]();
//
//  /** All expected subtypes should be registered. */
//  def register[Subtype<:This](name : String)
//  (implicit mf : ClassManifest[Subtype], rw : ReadWritable[Subtype]) : Unit = {
//    if (registry.contains(name)) {
//      throw new TypedCompanionException("Name '"+name+"' already registered.");
//    }
//    registry(name) = (mf, rw);
//  }
//
//  override implicit val readWritable : ReadWritable[This] = new ReadWritable[This] {
//    override def read(in : Input) : This = {
//      val name : String = readName(in);
//      val rw = registry.getOrElse(name,
//        throw new TypedCompanionException("No companion registered for '"+name+"'"))._2;
//      val rv = rw.read((name.iterator ++ in).buffered).asInstanceOf[This];
//
//      TextSerialization.skipWhitespace(in);
//      if (in.head == '~' || in.head == 'a') {
//        if (in.head == '~') {
//          TextSerialization.expect(in, "~>", false);
//        } else if (in.head == 'a') {
//          TextSerialization.expect(in, "andThen", false);
//        }
//        TextSerialization.skipWhitespace(in);
//        Function1Companion[B,_]
//      }
//      if (TextSerialization)
//      return continueParsing(in, rv);
//    }
//
//    override def write(out : Output, value : This) = {
//      val valType = value.asInstanceOf[AnyRef].getClass;
//      val candidates = registry.valuesIterator.
//        filter(tup => tup._1.erasure.isAssignableFrom(valType));
//
//      if (!candidates.hasNext) {
//        throw new TypedCompanionException("No registered handler supports value type "+valType);
//      }
//
//      val rw = candidates.
//        reduceLeft((tupA,tupB) => if (tupA._1 <:< tupB._1) tupA else tupB).
//        _2.asInstanceOf[ReadWritable[This]];
//
//      rw.write(out, value);
//    }
//  }
//
//}
