package scalanlp.classify;

import counters._;
import data._;

trait Classifier[L,Fk,-Fv] extends (Observation[Fk,Fv]=>L) {
  def apply(o :Observation[Fk,Fv]) = classify(o);
  def classify(o :Observation[Fk,Fv]) = scores(o).argmax;
  def scores(o: Observation[Fk,Fv]): DoubleCounter[L];
}
