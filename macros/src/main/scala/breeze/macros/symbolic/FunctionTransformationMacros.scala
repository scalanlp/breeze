package breeze.macros.symbolic

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import scala.collection.JavaConverters._
import org.matheclipse.core.expression._
import F.{Map => _, List => _, _}
import org.matheclipse.core.interfaces.IExpr
import org.matheclipse.core.eval.EvalEngine
import org.matheclipse.core.interfaces.{ISymbol => SymjaSymbol}
import org.matheclipse.core.expression.{Symbol => SymjaSymbolImpl}
import org.matheclipse.core.generic.Functors

import scala.util.Random
/**
  * Contains macros that transform SymbolicFunctions to an AST of the
  * computational algebra library Symja, perform derivation / simplification
  * in Symja and convert the result back to SymbolicFunctions.
  * Used by CanDerive and CanSimplify to create implicit typeclass instances.
  */
class FunctionTransformationMacros(val c: whitebox.Context) {

  F.initSymbols()

  import c.mirror.universe._

  private lazy val HListConsClassSymbol = c.mirror.staticClass("shapeless.$colon$colon")
  private lazy val HNilClassSymbol = c.mirror.staticClass("shapeless.HNil")
  private lazy val evalEngine = new EvalEngine()
  private case class ValueAndType(value: Tree, tpe: Tree)
  object Extractors {
    case class Extractor[O](pf: PartialFunction[IExpr, O]) {
      def unapply(i: IExpr): Option[O] = pf.lift.apply(i)
    }

    case class SeqExtractor[O](pf: PartialFunction[IExpr, Seq[O]]) {
      def unapplySeq(i: IExpr): Option[Seq[O]] = pf.lift.apply(i)
    }

    val Times = SeqExtractor[IExpr] { case e if e.isTimes => e.leaves.asScala }
    val Plus = SeqExtractor[IExpr] { case e if e.isPlus => e.leaves.asScala }
    val Exponential = Extractor[IExpr] { case e if e.isPower && e.getAt(1).equals(F.E) => e.getAt(2) }
    val Power = Extractor[(IExpr, IExpr)] { case e if e.isPower => (e.getAt(1), e.getAt(2)) }
    val Logarithm = Extractor[IExpr] { case e if e.isLog => e.getAt(1) }
    val Symbol = Extractor[SymjaSymbol] { case e if e.isSymbol => e.asInstanceOf[SymjaSymbol] }
    val ConstInt = Extractor[Double] { case e if e.isInteger => e.asInstanceOf[IntegerSym].doubleValue() }
    val Derivation = Extractor[IExpr] { case e if e.isAST("D", 3) => e.getAt(1).getAt(0) }
    val Numeric = Extractor[Double] { case e if e.isNumeric => e.asInstanceOf[Num].doubleValue() }
    val Function = Extractor[SymjaSymbol] {
      case e: AST if e.head().asInstanceOf[SymjaSymbolImpl].getSymbol.startsWith("fun") => e.head().asInstanceOf[SymjaSymbol]
    }
  }

  case class SymbolFunction(name: String) {
    def unapply(sym: Symbol): Option[Symbol] =
      if (sym == c.mirror.staticClass(s"breeze.symbolic.$name"))
        Some(sym)
      else
        None
  }

  val ExponentialFunc = SymbolFunction("Exponential")
  val PowerFunc = SymbolFunction("Power")
  val LogarithmFunc = SymbolFunction("Logarithm")
  val SumFunc = SymbolFunction("Sum")
  val DifferenceFunc = SymbolFunction("Difference")
  val ProductFunc = SymbolFunction("Product")
  val DivisionFunc = SymbolFunction("Division")
  val ConstOneFunc = SymbolFunction("ConstOne")
  val ConstFunc = SymbolFunction("Const")
  val NamedVarFunc = SymbolFunction("NamedVar")
  val VarFunc = SymbolFunction("Var")
  val ChainFunc = SymbolFunction("Chain")

  private case class SymjaExprWithSymbolTable(symjaExpr: IExpr, symbolTable: Map[SymjaSymbol, (Type, List[String])]) {

    def withExpression(expr: IExpr) = copy(symjaExpr = expr)

    def withMappedExpression(wrapFn: IExpr => IExpr) = withExpression(wrapFn(symjaExpr))

    def withPrependedPath(path: String): SymjaExprWithSymbolTable =
      copy(symbolTable = symbolTable.mapValues { case (t, l) => (t, path :: l) })

    def mergeWith(otherExpr: SymjaExprWithSymbolTable, combiner: (IExpr, IExpr) => IExpr): SymjaExprWithSymbolTable =
      copy(symjaExpr = combiner(symjaExpr, otherExpr.symjaExpr), symbolTable = symbolTable ++ otherExpr.symbolTable)

    def pathAndTypeOfExpression: Option[(Type, List[String])] = symjaExpr match {
      case s: SymjaSymbol => symbolTable.get(s)
      case a: AST if a.head().isSymbol => symbolTable.get(a.head().asInstanceOf[SymjaSymbol])
      case _ => None
    }
  }

  private object CasFunction {
    def unapplySeq(typeArgs: List[Type]): Option[List[SymjaExprWithSymbolTable]] =
      Some(typeArgs.map(ta => toCasFunction(ta)))
  }

  val defaultFunctionArgument: SymjaSymbol = F.x

  private def toCasFunction(inner: Type): SymjaExprWithSymbolTable =
    inner.dealias match {
      case TypeRef(pre, sym, args) => {
        (sym, args) match {
          case (ExponentialFunc(_), CasFunction(symjaExprWithSymbolTable)) =>
            symjaExprWithSymbolTable.withPrependedPath("fn").withMappedExpression(Exp(_))

          case (PowerFunc(_), Seq(base, exponent)) => {
            val expCas = toCasFunction(exponent).withPrependedPath("exp")
            toCasFunction(base).withPrependedPath("fn").mergeWith(expCas, (base, exp) => Power(base, exp))
          }

          case (LogarithmFunc(_), CasFunction(symjaExprWithSymbolTable)) =>
            symjaExprWithSymbolTable.withPrependedPath("fn").withMappedExpression(Log(_))

          case (SumFunc(_), Seq(funcHList)) =>
            extractHList(funcHList).reduce { (symA, symB) =>
              symA.mergeWith(symB, _ plus _)
            }.withPrependedPath("fns")

          case (DifferenceFunc(_), CasFunction(numerator, denominator)) =>
            numerator.withPrependedPath("fn1").mergeWith(denominator.withPrependedPath("fn2"), _ minus _)

          case (ProductFunc(_), Seq(funcHList)) =>
            extractHList(funcHList).reduce { (symA, symB) =>
              symA.mergeWith(symB, _ times _)
            }.withPrependedPath("fns")

          case (DivisionFunc(_), CasFunction(numerator, denominator)) =>
            numerator.withPrependedPath("fn1").mergeWith(denominator.withPrependedPath("fn2"), _ divide  _)

          case (ConstOneFunc(_), Seq()) =>
            SymjaExprWithSymbolTable(Num.valueOf(1.0), Map())

          case (ConstFunc(_), Seq(innerType)) => {
            val variable = new SymjaSymbolImpl("var" + Random.nextLong.abs.toString)
            SymjaExprWithSymbolTable(variable, Map(variable -> (inner, List())))
          }

          case (NamedVarFunc(_), Seq(innerType)) => {
            val varName = innerType match {
              case TypeRef(SingleType(_, sym), _, _) => sym.name.toString
              case RefinedType(bar, _) =>
                val List(_, TypeRef(_, _, List(constant))) = bar
                val ConstantType(Constant(sym)) = constant
                sym.toString
            }
            val variable = new SymjaSymbolImpl(varName)
            SymjaExprWithSymbolTable(variable, Map(variable -> (inner, List())))
          }

          case (VarFunc(_), Seq()) =>
            SymjaExprWithSymbolTable(defaultFunctionArgument, Map(defaultFunctionArgument -> (inner, List[String]())))

          case (ChainFunc(_), CasFunction(outerFunc, innerFunc)) =>
            outerFunc.withPrependedPath("outer").mergeWith(innerFunc.withPrependedPath("inner"), (o, i) => F.subst(o, Functors.rules(Rule(defaultFunctionArgument, i))))
          case _ =>
            val functionName = F.local(s"fun${inner.hashCode}")
            SymjaExprWithSymbolTable(F.unary(functionName, defaultFunctionArgument), Map(functionName -> (inner, List())))
        }
      }
      case x => c.abort(c.enclosingPosition, s"Got something else\n$inner")
    }

  private def extractHList(hlistType: Type): List[SymjaExprWithSymbolTable] = {
    hlistType.dealias match {
      case TypeRef(_, sym, _) if sym == HNilClassSymbol => Nil
      case TypeRef(_, sym, Seq(head, tail)) if sym == HListConsClassSymbol =>
        toCasFunction(head).withPrependedPath("head") :: extractHList(tail).map(_.withPrependedPath("tail"))
    }
  }

  private def toHList(exps: Seq[SymjaExprWithSymbolTable]): ValueAndType =
    exps.map(v => toSymbolicFunction(v))
      .foldLeft(ValueAndType(q"shapeless.HNil", tq"shapeless.HNil")) { case (ValueAndType(listValue, listType), ValueAndType(elemValue, elemType)) =>
        ValueAndType(q"$elemValue::$listValue", tq"shapeless.::[$elemType,$listType]")
      }


  private def toSymbolicFunction(expr: SymjaExprWithSymbolTable): ValueAndType = expr.symjaExpr match {
    case Extractors.Times(vs@_*) => {
      val ValueAndType(factorValues, factorTypes) = toHList(vs.map(expr.withExpression))
      ValueAndType(q"breeze.symbolic.Product.apply($factorValues)", tq"breeze.symbolic.Product[$factorTypes]")
    }

    case Extractors.Plus(vs@_*) => {
      val ValueAndType(summandValues, summandTypes) = toHList(vs.map(expr.withExpression))
      ValueAndType(q"breeze.symbolic.Sum.apply($summandValues)", tq"breeze.symbolic.Sum[$summandTypes]")
    }

    case Extractors.Exponential(innerExp) => {
      val ValueAndType(innerSymbolic, innerType) = toSymbolicFunction(expr.withExpression(innerExp))
      ValueAndType(q"breeze.symbolic.Exponential.apply($innerSymbolic)", tq"breeze.symbolic.Exponential[$innerType]")
    }

    case Extractors.Logarithm(innerExp) => {
      val ValueAndType(innerSymbolic, innerType) = toSymbolicFunction(expr.withExpression(innerExp))
      ValueAndType(q"breeze.symbolic.Logarithm.apply($innerSymbolic)", tq"breeze.symbolic.Logarithm[$innerType]")
    }

    case Extractors.Symbol(_) => {
      val Some((tpe, path)) = expr.pathAndTypeOfExpression
      ValueAndType(path.tail.foldLeft(Ident(TermName(path.head)): Tree) { (lo, selector) =>
        Select(qualifier = lo, name = TermName(selector))
      }, tq"$tpe")
    }

    case Extractors.Power(innerExp, exponent) => {
      val ValueAndType(innerSymbolic, innerType) = toSymbolicFunction(expr.withExpression(innerExp))
      val ValueAndType(expSymbolic, expType) = toSymbolicFunction(expr.withExpression(exponent))
      ValueAndType(q"breeze.symbolic.Power.apply[$innerType, $expType]($innerSymbolic, $expSymbolic)", tq"breeze.symbolic.Power[$innerType, $expType]")
    }

    case Extractors.ConstInt(value) => {
      if (value == 0)
        ValueAndType(q"breeze.symbolic.ConstZero.apply()", tq"breeze.symbolic.ConstZero")
      else if (value == 1)
        ValueAndType(q"breeze.symbolic.ConstOne.apply()", tq"breeze.symbolic.ConstOne")
      else
        ValueAndType(q"breeze.symbolic.Const.apply[Double]($value)", tq"breeze.symbolic.Const[Double]")
    }

    case Extractors.Derivation(innerExp) => {
      val ValueAndType(innerSymbolic, innerType) = toSymbolicFunction(expr.withExpression(innerExp))
      ValueAndType(q"breeze.symbolic.Derivation.apply[$innerType]($innerSymbolic)", tq"breeze.symbolic.Derivation[$innerType]")
    }

    case Extractors.Function(_) => {
      val Some((tpe, path)) = expr.pathAndTypeOfExpression
      ValueAndType(path.tail.foldLeft(Ident(TermName(path.head)): Tree) { (lo, selector) =>
        Select(qualifier = lo, name = TermName(selector))
      }, tq"$tpe")
    }

    case Extractors.Numeric(value) => {
      if (value == 0)
        ValueAndType(q"breeze.symbolic.ConstZero.apply()", tq"breeze.symbolic.ConstZero")
      else if (value == 1)
        ValueAndType(q"breeze.symbolic.ConstOne.apply()", tq"breeze.symbolic.ConstOne")
      else
        ValueAndType(q"breeze.symbolic.Const.apply[Double]($value)", tq"breeze.symbolic.Const[Double]")
    }

    case x => c.abort(c.enclosingPosition, s"Don't know how to transform $x into a breeze.symbolic.SymbolicFunction")
  }

  private def transform[SF: c.WeakTypeTag](inputType: Type)(transformation: IExpr => IExpr): ValueAndType = {
    val res = toCasFunction(inputType)
    val transformed = res.withPrependedPath("f").withMappedExpression(transformation)
    toSymbolicFunction(transformed)
  }

  def canDerive_impl[SF: c.WeakTypeTag]: c.Tree = {
    val inputType = c.weakTypeTag[SF].tpe
    val ValueAndType(value, outputType) = transform(inputType) { expr =>
      evalEngine.evaluate(Simplify(D(expr, defaultFunctionArgument)))
    }
    q"""
      new breeze.symbolic.CanDerive[$inputType] {
        type D = $outputType
        def derivation(f: $inputType): D = {
          $value
        }
      }
    """
  }

  def canSimplify_impl[SF: c.WeakTypeTag]: c.Tree = {
    val inputType = c.weakTypeTag[SF].tpe
    val ValueAndType(value, outputType) = transform(inputType) { expr =>
      evalEngine.evaluate(Simplify(expr))
    }
    q"""
      new breeze.symbolic.CanSimplify[$inputType] {
        type R = $outputType
        def simplify(f: $inputType): R = {
          $value
        }
      }
    """
  }

}
