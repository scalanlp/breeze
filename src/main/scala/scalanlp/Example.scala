package scalanlp.data;

import scala.collection.Map;
import counters._;

/**
* Represents a single example from a collection of data. Intentionally overly general.
*
* @author dlwh
*/
trait Example[L,Fk,+Fv] extends MultilabeledExample[L,Fk,Fv] with Labeled[L] {
  def id : String;
  def label: L
  // How do we marry real-valued features and categorial features? By being overly general.
  def features: Map[Fk, Fv];
  override def toString = {
    "Example { ids =" + id + ", labels = " + labels + ", features = " + features + "}"; 
  }
}

object Example {
  type RealValued[L,Fk] = Example[L,Fk,Double];
  type IntValued[L,Fk] = Example[L,Fk,Int];

  def apply[L,F](id_ : String, label_ : L, features_ : IntCounter[F]) = new IntValued[L,F] {
    def id = id_;
    override def label = label_;
    def features = features_;
  }

  def apply[L,F](label_ : L, features_ : IntCounter[F]):IntValued[L,F] = apply("",label_ , features_);
}
