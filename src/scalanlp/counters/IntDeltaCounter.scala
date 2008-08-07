package scalanlp.counters;
import scala.collection.mutable.HashMap;

/**
* Uses a pre-existing counter for default values. 
* Any updates to the first counter may or may not be reflected.
* @author(dlwh)
*/
@serializable
class IntDeltaCounter[K1](origin : IntCounter[K1]) extends HashMap[K1,Int] with IntCounter[K1] {
  override def default(k1 : K1) :Int = origin(k1);
}
