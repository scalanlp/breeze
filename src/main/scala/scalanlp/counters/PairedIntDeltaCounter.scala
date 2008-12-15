package scalanlp.counters;
import scala.collection.mutable.HashMap;

/**
* Uses a pre-existing counter for default values. 
* Any updates to the first counter may or may not be reflected.
* @author(dlwh)
*/
@serializable
class PairedIntDeltaCounter[K1,K2](origin : PairedIntCounter[K1,K2]) extends PairedIntCounter[K1,K2] {
  // may be overridden with other counters
  override def default(k1 : K1) :IntCounter[K2] = new IntDeltaCounter[K2](origin(k1));
}

