package breeze.macros

import scala.annotation.{Annotation, StaticAnnotation, compileTimeOnly}

/**
 * expand is a macro annotation that is kind of like @specialized, but it's more of a templating mechanism.
 * It is pretty... alpha in that the functionality is basically there, but it is now in the least bit battle tested.
 * Don't ask much of it, and it will do fine, ask a lot, and well...
 *
 * Basically, expand takes a def with type arguments whose types are annotated with [[breeze.macros.expand.args]]
 * and generates the cross product of all combinations. For example:
 *
 * {{{
 *   @expand
 *   def foo[@expandArgs(Int, Double) T, @expandArgs(Int, Double) U](x: T, y: U) = x + y
 * }}}
 *
 * will generate
 * {{{
 *   def foo_T_Int_U_Int(x: Int, y: Int) = x + y
 *   def foo_T_Int_U_Double(x: Int, y: Double) = x + y
 *   def foo_T_Double_U_Int(x: Double, y: Int) = x + y
 *   def foo_T_Double_U_Double(x: Double, y: Double) = x + y
 * }}}
 *
 * The real power comes from [[breeze.macros.expand.sequence]], which annotates an argument to the method
 * to correlate with a type (the first argument to sequence) and then a sequence of trees which are inlined
 * in place of references to the argument. For example:
 *
 * {{{
 *   @expand
 *   def foo[@expandArgs(Int, Double) T](x: T, y: T)(implicit @sequence(T)({_ + _}, {_ * _}) op: XXX) = op(x,y)
 *   /* The type of op is unimportant, though giving it a "real" type is useful. */
 * }}}
 *
 * will generate
 * {{{
 *   def foo_T_Int(x: Int, y: Int) = x + y
 *   def foo_T_Double(x: Double, y: Double) = x * y
 * }}}
 *
 *
 * See [[breeze.linalg.DenseVectorOps]] for a more complete example.
 *
 *
 *@author dlwh
 **/
@compileTimeOnly("Use the breeze-codegen-expand plugin")
class expand extends Annotation with StaticAnnotation {
}

object expand {

  /** Args are put on type arguments, and the cross product of all types that are so annotated
   * are instantiated.
   * @param args
   */
  class args(args: Any*) extends Annotation with StaticAnnotation

  /** Excludes specific instantiations of the cross product inferred by @args.
   * Order is the same as the order of the type arguments */
  class exclude(args: Any*) extends Annotation with StaticAnnotation

  /** Replaces a def with a val. Requires that all type arguments be expanded and all term arguments be sequenced */
  class valify extends Annotation with StaticAnnotation

  /** \@sequence[T](args) associates the term parameter's values with the type argument indicated. */
  class sequence[T](args: Any*) extends Annotation with StaticAnnotation

}
