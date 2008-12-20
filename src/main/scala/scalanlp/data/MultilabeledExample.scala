package scalanlp.data;

import scala.collection.Map;

/**
* Represents a single example from a collection of data. Intentionally
* overly general.
*
* @author dlwh
*/
trait MultilabeledExample[L,Fk,+Fv] extends Observation[Fk,Fv] with Multilabeled[L] {
  def id : String;
  // How do we marry real-valued features and categorial features? By being overly general.
  def features: Map[Fk, Fv];
  override def toString = {
    "Example { ids =" + id + ", labels = " + labels + ", features = " + features + "}"; 
  }
}
