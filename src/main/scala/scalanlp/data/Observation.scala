package scalanlp.data;

import scala.collection.Map;
import counters._;

/**
* Represents a single unlabeled example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
trait Observation[+T] { outer=>
  def id : String;
  def features: T;

  def map[U](f: T=>U) = new Observation[U] {
    def id = outer.id;
    lazy val features = f(outer.features);
  }

  def flatMap[U](f: T=>U) = map(f);

  override def toString = {
    "Observation { ids =" + id + ", features = " + features + "}"; 
  }
}

object Observation {
  def apply[T]( _id: String, what: T ) = new Observation[T] {
    def id = _id;
    def features = what;
  }
}
