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

import scala.collection.mutable.{HashMap,ArrayBuffer};

import scala.reflect.ClassManifest;

import scalanlp.ra.ReflectionUtils;

import TextSerialization._;

///**
// * Registers all TypedCompanion instances to enable subtype instantion.
// *
// * @author dramage
// */
//object TypedCompanionRegistry {
//  val registry = HashMap[Class[_], ArrayBuffer[(String,TypedCompanion[_,_])]]();
//
//  def register(t : Class[_], name : String, cc : TypedCompanion[_,_]) : Unit = {
//    registry.getOrElseUpdate(t, ArrayBuffer[(String,TypedCompanion[_,_])]()) += ((name, cc));
//  }
//}

/**
 * Mix-in trait for companion object to case classes to automatically
 * support {@link TextSerialization} toString and fromString.
 *
 * @author dramage
 */
trait TypedCompanion[Components,This] {
  private var _manifest : Option[ClassManifest[This]] = None;
  private var _components : Option[Components] = None;

  /** Manifest for This type. This must be defined in advance. */
  def manifest : ClassManifest[This] = _manifest match {
    case Some(m) => m;
    case _ => throw new TypedCompanionException("No manifest specified in TypedCompanion.");
  }

  protected def manifest_=(m : ClassManifest[This]) {
    if (_manifest.isDefined && _manifest.get.toString != m.toString) {
      throw new TypedCompanionException("Manifest already defined in TypeCompanion.");
    }
    _manifest = Some(m);
//    for (cls <- ReflectionUtils.getSupertypes(manifest.erasure)) {
//      TypedCompanionRegistry.register(cls, name, this);
//    }
  }

  /** ReadWritable for each component needed during building. */
  def components : Components = _components match {
    case Some(c) => c;
    case _ => throw new TypedCompanionException("No components specified in TypedCompanion.");
  }

  protected def components_=(c : Components) {
    if (_components.isDefined && _components.get != c) {
      throw new TypedCompanionException("Components already defined.")
    }
    _components = Some(c);
  }

  /** A ReadWritable for the primary type This. */
  implicit def readWritable : ReadWritable[This];

  /** Returns the name of the primary (non-companion) class. */
  def name =
    manifest.erasure.getSimpleName;
}

/**
 * Mix-in trait for companion object to a type that takes no arguments
 * to instantiate for automatic support of {@link TextSerialization}
 * toString and fromString.
 *
 * @author damage
 */
trait TypedCompanion0[This] extends TypedCompanion[Unit,This] {
  /** Static constructor. */
  def apply() : This;

  def prepare()(implicit m : ClassManifest[This]) =
    manifest = m;

  override implicit val readWritable = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, "()", false);
      apply();
    }

    override def write(out : Output, value : This) = {
      out.append(name);
      out.append("()");
    }
  }
}

/**
 * Mix-in trait for companion object to case classes to automatically
 * support TextSerialization toString and fromString.
 *
 * @author dramage
 */
trait TypedCaseCompanion1[P1,This]
extends TypedCompanion[ReadWritable[P1],This] {
  /** Static constructor. */
  def apply(p1 : P1) : This;

  def prepare()(implicit m : ClassManifest[This], p1H : ReadWritable[P1]) = {
    manifest = m;
    components = p1H;
  }

  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def unpack(t : This) : P1 = {
    val name = t.asInstanceOf[AnyRef].getClass.getDeclaredFields()(0).getName;
    t.asInstanceOf[AnyRef].getClass.getMethod(name).invoke(t).asInstanceOf[P1];
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  override implicit val readWritable = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      val p1 = components.read(in);
      expect(in, ')', false);
      apply(p1);
    }

    override def write(out : Output, value : This) = {
      out.append(name);
      out.append('(');
      components.write(out, unpack(value));
      out.append(')');
    }
  }
}



/**
 * Mix-in trait for companion object to case classes to automatically
 * support {@link TextSerialization} toString and fromString.
 *
 * @author dramage
 */
trait TypedCaseCompanion2[P1,P2,This]
extends TypedCompanion[(ReadWritable[P1],ReadWritable[P2]),This] {
  /** Static constructor. */
  def apply(p1 : P1, p2 : P2) : This;

  def prepare()(implicit m : ClassManifest[This], p1H : ReadWritable[P1], p2H : ReadWritable[P2]) {
    manifest = m;
    components = (p1H, p2H);
  }

  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def unpack(t : This) : (P1,P2) = {
    val n1 = t.asInstanceOf[AnyRef].getClass.getDeclaredFields()(1).getName;
    val p1 = t.asInstanceOf[AnyRef].getClass.getMethod(n1).invoke(t).asInstanceOf[P1];
    val n2 = t.asInstanceOf[AnyRef].getClass.getDeclaredFields()(0).getName;
    val p2 = t.asInstanceOf[AnyRef].getClass.getMethod(n2).invoke(t).asInstanceOf[P2];
    (p1,p2);
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  override implicit val readWritable = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      val p1 = components._1.read(in);
      expect(in, ',', false);
      val p2 = components._2.read(in);
      expect(in, ')', false);
      apply(p1, p2);
    }

    override def write(out : Output, value : This) = {
      out.append(name);
      out.append('(');
      val (p1,p2) = unpack(value);
      components._1.write(out, p1);
      out.append(',');
      components._2.write(out, p2);
      out.append(')');
    }
  }
}

/**
 * Mix-in trait for companion objects to generic supertypes of TypedCompanions
 * that will instantiate the correct subtype (from a fixed registry) at
 * runtime.
 */
trait SubtypedCompanion[This] extends TypedCompanion[Unit,This] {
  /** Registry of known sub-types. */
  val registry = HashMap[String, (ClassManifest[_],ReadWritable[_])]();

  /** This needs to be called first. */
  def prepare()(implicit m : ClassManifest[This]) =
    manifest = m;

  /** All expected subtypes should be registered. */
  def register[Subtype<:This](name : String)
  (implicit mf : ClassManifest[Subtype], rw : ReadWritable[Subtype]) : Unit = {
    if (registry.contains(name)) {
      throw new TypedCompanionException("Name '"+name+"' already registered.");
    }
    registry(name) = (mf, rw);
  }

  override implicit def readWritable = _readWritable;

  private val _readWritable = mkReadWritable;
  private def mkReadWritable : ReadWritable[This] = new ReadWritable[This] {
    override def read(in : Input) : This = {
      val name : String = readName(in);
      val rw = registry.getOrElse(name,
        throw new TypedCompanionException("No companion registered for '"+name+"'"))._2;
      return rw.read((name.iterator ++ in).buffered).asInstanceOf[This];
    }

    override def write(out : Output, value : This) = {
      val valType = value.asInstanceOf[AnyRef].getClass;
      val candidates = registry.valuesIterator.
        filter(tup => tup._1.erasure.isAssignableFrom(valType));

      if (!candidates.hasNext) {
        throw new TypedCompanionException("No registered handler supports value type "+valType);
      }

      val rw = candidates.
        reduceLeft((tupA,tupB) => if (tupA._1 <:< tupB._1) tupA else tupB).
        _2.asInstanceOf[ReadWritable[This]];
      
      rw.write(out, value);
      
//      val name : String = try {
//        value.asInstanceOf[AnyRef].getClass.getClassLoader.
//          loadClass(value.asInstanceOf[AnyRef].getClass.getName+"$").
//          getMethod("name").invoke(null).asInstanceOf[String];
//      } catch {
//        case e : Throwable =>
//          throw new TypedCompanionException("Unable to get name from expected "+
//           "companion object "+value.asInstanceOf[AnyRef].getClass.getName+"$");
//      }
//
//      val tc = registry.getOrElse(name,
//        throw new TypedCompanionException("No companion registered fro '"+name+"'")
//      ).asInstanceOf[TypedCompanion[_,This]];
//
//      tc.readWritable.write(out, value);
    }
  }
}

/** Exception thrown by an improperly configured TypedCompanion. */
class TypedCompanionException(msg : String, cause : Throwable)
extends RuntimeException(msg, cause) {
  def this(msg : String) = this(msg, null);
}
