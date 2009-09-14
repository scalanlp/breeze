package scalara.ra

/**
 * A mix-in trait that provides a short signature of an object based on
 * the hash of its toString.  This method should only be mixed in to
 * immutable classes with a reasonable toString defined.
 * 
 * @author dramage
 */
trait Signature {
  def signature : String = {
    val hex = this.toString.hashCode.toHexString;
    "0"*(8-hex.length)+hex;
  }
}
