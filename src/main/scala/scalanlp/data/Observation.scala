package scalanlp.data;

import scala.collection.Map;
import counters._;

/**
* Represents a single unlabeled example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
trait Observation[Fk,+Fv] {
  def id : String;
  // How do we marry real-valued features and categorial features? By being overly general.
  def features: Map[Fk, Fv];
  override def toString = {
    "Observation { ids =" + id + ", features = " + features + "}"; 
  }
}

