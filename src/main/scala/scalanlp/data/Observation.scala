package scalanlp.data;

import scala.collection.Map;
import counters._;

/**
* Represents a single unlabeled example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
@serializable
trait Observation[+T] { outer=>
  def id : String;
  def features: T;

  /**
  * non-strict, but cached, transformation of features
  */
  def map[U](f: T=>U) = new Observation[U] {
    def id = outer.id;
    lazy val features = f(outer.features);
  }

  /**
  * non-strict, but cached, transformation of features
  */
  def flatMap[U](f: T=>U) = map(f);

  override def toString = {
    "Observation { ids =" + id + ", features = " + features + "}"; 
  }
}

object Observation {
  def apply[T](_id: String, _features: T) = new Observation[T] {
   val id = _id;
   val features = _features;
  }
}
