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

import scala.collection.mutable.HashMap;

import scala.reflect.ClassManifest;

import TextSerialization._;

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

object TypedCompanion {
  val paranamer = new com.thoughtworks.paranamer.BytecodeReadingParanamer();
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
      if (in.hasNext && in.head == '(') {
        expect(in, "()", false);
      }
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
 * support {@link TextSerialization} toString and fromString.
 * 
 * Subtypes can provide an unpack method that breaks apart the case class into
 * its companions.  The default behavior is based on Scala 2.8's case class
 * encoding.
 *
 * @author dramage
 */
trait TypedCompanion1[P1,This]
extends TypedCompanion[ReadWritable[P1],This] {
  /** Static constructor. */
  def apply(p1 : P1) : This;

  protected def prepare()(implicit m : ClassManifest[This], p1H : ReadWritable[P1]) = {
    manifest = m;
    components = p1H;
  }

  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def unpack(t : This) : P1 = {
    try {
      val constructor = t.asInstanceOf[AnyRef].getClass.getConstructors()(0);
      val names = TypedCompanion.paranamer.lookupParameterNames(constructor);
      t.asInstanceOf[AnyRef].getClass.getMethod(names(0)).invoke(t).asInstanceOf[P1];
    } catch {
      case ex : Throwable => throw new TypedCompanionException(
        "Could not automatically recover components of "+
        t.asInstanceOf[AnyRef].getClass+": you must provide a custom "+
        "unpack() implementation in "+this.getClass, ex);
        
    }
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  override implicit val readWritable = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      skipWhitespace(in);
      val p1 = components.read(in);
      skipWhitespace(in);
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
 * Subtypes can provide an unpack method that breaks apart the case class into
 * its companions.  The default behavior is based on Scala 2.8's case class
 * encoding.
 *
 * @author dramage
 */
trait TypedCompanion2[P1,P2,This]
extends TypedCompanion[(ReadWritable[P1],ReadWritable[P2]),This] {
  /** Static constructor. */
  def apply(p1 : P1, p2 : P2) : This;

  protected def prepare()(implicit m : ClassManifest[This], p1H : ReadWritable[P1], p2H : ReadWritable[P2]) {
    manifest = m;
    components = (p1H, p2H);
  }


  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def unpack(t : This) : (P1,P2) = {
    try {
      val constructor = t.asInstanceOf[AnyRef].getClass.getConstructors()(0);
      val names = TypedCompanion.paranamer.lookupParameterNames(constructor);
      val p1 = t.asInstanceOf[AnyRef].getClass.getMethod(names(0)).invoke(t).asInstanceOf[P1];
      val p2 = t.asInstanceOf[AnyRef].getClass.getMethod(names(1)).invoke(t).asInstanceOf[P2];
      (p1,p2);
    } catch {
      case ex : Throwable => throw new TypedCompanionException(
        "Could not automatically recover components of "+
        t.asInstanceOf[AnyRef].getClass+": you must provide a custom "+
        "unpack() implementation in "+this.getClass, ex);
    }
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  override implicit val readWritable = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      skipWhitespace(in);
      val p1 = components._1.read(in);
      skipWhitespace(in);
      expect(in, ',', false);
      skipWhitespace(in);
      val p2 = components._2.read(in);
      skipWhitespace(in);
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
 * Mix-in trait for companion object to case classes to automatically
 * support {@link TextSerialization} toString and fromString.
 *
 * Subtypes can provide an unpack method that breaks apart the case class into
 * its companions.  The default behavior is based on Scala 2.8's case class
 * encoding.
 *
 * @author dramage
 */
trait TypedCompanion3[P1,P2,P3,This]
extends TypedCompanion[(ReadWritable[P1],ReadWritable[P2],ReadWritable[P3]),This] {
  /** Static constructor. */
  def apply(p1 : P1, p2 : P2, p3 : P3) : This;

  protected def prepare()(implicit m : ClassManifest[This], p1H : ReadWritable[P1], p2H : ReadWritable[P2], p3H : ReadWritable[P3]) {
    manifest = m;
    components = (p1H, p2H, p3H);
  }


  /**
   * Returns the arguments given to the apply() static constructor.  This
   * method depends on the particulars of case class encoding.
   */
  def unpack(t : This) : (P1,P2, P3) = {
    try {
      val constructor = t.asInstanceOf[AnyRef].getClass.getConstructors()(0);
      val names = TypedCompanion.paranamer.lookupParameterNames(constructor);
      val p1 = t.asInstanceOf[AnyRef].getClass.getMethod(names(0)).invoke(t).asInstanceOf[P1];
      val p2 = t.asInstanceOf[AnyRef].getClass.getMethod(names(1)).invoke(t).asInstanceOf[P2];
      val p3 = t.asInstanceOf[AnyRef].getClass.getMethod(names(2)).invoke(t).asInstanceOf[P3];
      (p1,p2,p3);
    } catch {
      case ex : Throwable => throw new TypedCompanionException(
        "Could not automatically recover components of "+
        t.asInstanceOf[AnyRef].getClass+": you must provide a custom "+
        "unpack() implementation in "+this.getClass, ex);
    }
  }

  /**
   * Constructs a ReadWritable for the primary type T.
   */
  override implicit val readWritable = new ReadWritable[This] {
    override def read(in : Input) = {
      expect(in, name, false);
      expect(in, '(', false);
      skipWhitespace(in);
      val p1 = components._1.read(in);
      skipWhitespace(in);
      expect(in, ',', false);
      skipWhitespace(in);
      val p2 = components._2.read(in);
      skipWhitespace(in);
      expect(in, ')', false);
      val p3 = components._3.read(in);
      skipWhitespace(in);
      expect(in, ')', false);
      apply(p1, p2, p3);
    }

    override def write(out : Output, value : This) = {
      out.append(name);
      out.append('(');
      val (p1,p2,p3) = unpack(value);
      components._1.write(out, p1);
      out.append(',');
      components._2.write(out, p2);
      out.append(',');
      components._3.write(out, p3);
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
  protected val registry = HashMap[String, (ClassManifest[_],ReadWritable[_])]();

  /** This needs to be called first. */
  protected def prepare()(implicit m : ClassManifest[This]) =
    manifest = m;

  /** All expected subtypes should be registered. */
  def register[Subtype<:This](name : String)
  (implicit mf : ClassManifest[Subtype], rw : ReadWritable[Subtype]) : Unit = {
    if (registry.contains(name)) {
      throw new TypedCompanionException("Name '"+name+"' already registered.");
    }
    registry(name) = (mf, rw);
  }

  /** Registers using the given type's simple name. */
  def register[Subtype<:This]()
  (implicit mf : ClassManifest[Subtype], rw : ReadWritable[Subtype]) : Unit = {
    register[Subtype](mf.erasure.getSimpleName);
  }

  /**
   * Sub-types can provide a mechanism to continue parsing input to build
   * a new (richer) version of This from the input stream.  By default, returns
   * current and does not modify input.
   */
  protected def continueParsing(input : TextSerialization.Input, current : This) : This =
    current;

  override implicit val readWritable : ReadWritable[This] = new ReadWritable[This] {
    override def read(in : Input) : This = {
      val name : String = readName(in);
      val rw = registry.getOrElse(name,
        throw new TypedCompanionException("No companion registered for '"+name+"'"))._2;
      val rv = rw.read((name.iterator ++ in).buffered).asInstanceOf[This];
      return continueParsing(in, rv);
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
    }
  }
}

/** Exception thrown by an improperly configured TypedCompanion. */
class TypedCompanionException(msg : String, cause : Throwable)
extends RuntimeException(msg, cause) {
  def this(msg : String) = this(msg, null);
}
