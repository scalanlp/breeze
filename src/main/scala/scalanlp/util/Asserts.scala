package scalanlp.util;

/**
* Collection of asserts that give more useful error messages.
*
* @author dlwh
*/
trait Asserts {
  /** == */
  def assertEq[A,B](a: A, b: B) = assert(a==b, a + "is not == " + b);
  /** != */
  def assertNe[A<%Ordered[B],B](a: A, b: B) = assert(a!=b, a + "is not != " + b);
  /** &lt; */
  def assertLt[A<%Ordered[B],B](a: A, b: B) = assert(a< b, a + "is not < " + b);
  /** &lt;= */
  def assertLe[A<%Ordered[B],B](a: A, b: B) = assert(a<=b, a + "is not <= " + b);
  /** &gt; */
  def assertGt[A<%Ordered[B],B](a: A, b: B) = assert(a> b, a + "is not > " + b);
  /** &gt;= */
  def assertGe[A<%Ordered[B],B](a: A, b: B) = assert(a>=b, a + "is not >= " + b);
}


/**
* Collection of asserts that give more useful error messages.
*
* @author dlwh
*/
object Asserts extends Asserts;
