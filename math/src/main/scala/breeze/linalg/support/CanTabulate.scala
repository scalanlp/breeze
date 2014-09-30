package breeze.linalg.support

/**
 * breeze
 * 7/19/14
 * @author Gabriel Schubiner <gabeos@cs.washington.edu>
 *
 *
 */
trait CanTabulate[I,V,E] {
  def apply(d: I, f: I => E): V
}
