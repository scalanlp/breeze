package scalanlp.math;

/**
* Utilities and implicits for iterators. Nothing major.
* @author dlwh
*/
object Arrays {
  def normalize(arr :Array[Double]) = {
    val c = arr.foldLeft(0.0)(_+_);
    if(c == 0) arr;
    else arr.map( _ / c).force;
  }
}
