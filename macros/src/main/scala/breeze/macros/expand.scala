package breeze.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.{ Annotation, StaticAnnotation }
import scala.collection.immutable.ListMap

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
class expand extends Annotation with StaticAnnotation {
  def macroTransform(annottees: Any*):Any = macro expand.expandImpl
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


  def stripOurAnnotations(c: Context)(mods: c.Modifiers):c.Modifiers = {
    import c.mirror.universe._

    mods.mapAnnotations(_.filter {
      case q"new expand.args(...$args)" => false
      case q"new expand.$x[..$targs](...$args)" => false
      case q"new expand.$x(...$args)" => false
      case q"new expand.$x" => false
      case _ => true
    })
  }

  def expandImpl(c: Context)(annottees: c.Expr[Any]*):c.Expr[Any] = {
    import c.mirror.universe._
    annottees.head.tree match {
      case tree@DefDef(mods, name, targs, vargs, tpt, rhs) =>

        val (typesToExpand, typesLeftAbstract) = targs.partition(shouldExpand(c)(_))

        val exclusions = getExclusions(c)(mods, targs.map(_.name))
        val shouldValify = checkValify(c)(mods)

        val typesToUnrollAs = ListMap.empty ++ typesToExpand.map { td =>
          (td.name: Name) -> typeMappings(c)(td)
        }

        val (valsToExpand, valsToLeave) = vargs.map(_.partition(shouldExpandVarg(c)(_))).unzip

        val valsToExpand2 = valsToExpand.flatten

        val configurations = makeTypeMaps(c)(typesToUnrollAs).filterNot(exclusions.toSet)
        val valExpansions = valsToExpand2.map{v => v.name -> solveSequence(c)(v, typesToUnrollAs)}.asInstanceOf[List[(c.Name, (c.Name, Map[c.Type, c.Tree]))]].toMap

        val newDefs = configurations.map{ typeMap =>
          val grounded = substitute(c)(typeMap, valExpansions, rhs)
          val newvargs = valsToLeave.filterNot(_.isEmpty).map(_.map(substitute(c)(typeMap, valExpansions, _).asInstanceOf[ValDef]))
          val newtpt = substitute(c)(typeMap, valExpansions, tpt)
          val newName = newTermName(mkName(c)(name, typesToExpand.map(t => typeMap(t.name).toString)))
          val newMods = stripOurAnnotations(c)(mods)
          if(shouldValify) {
            if(typesLeftAbstract.nonEmpty)
              c.error(tree.pos, "Can't valify: Not all types were grounded: " + typesLeftAbstract.mkString(", "))
            if(newvargs.exists(_.nonEmpty))
              c.error(tree.pos, "Can't valify: Not all arguments were grounded: " + newvargs.map(_.mkString(", ")).mkString("(",")(",")"))
            ValDef(newMods, newName, newtpt, grounded)
          } else {
            val newTargs = typesLeftAbstract.map(substitute(c)(typeMap, valExpansions, _)).asInstanceOf[List[TypeDef]]
            DefDef(newMods, newName, newTargs, newvargs, newtpt, grounded)
          }
        }
        val ret = c.Expr(Block(newDefs.toList, Literal(Constant(()))))
        ret
      case _ => ???
    }
  }

  private def mkName(c: Context)(name: c.Name, groundedTypes: Seq[String]): String = {
    groundedTypes.map {_.reverse.takeWhile(_ != '.').reverse }.mkString(name.toString + "_", "_", "")
  }

  // valExpansions is a [value identifier -> (
  def substitute(c: Context)(typeMap: Map[c.Name, c.Type], valExpansions: Map[c.Name, (c.Name, Map[c.Type, c.Tree])], rhs: c.mirror.universe.Tree): c.mirror.universe.Tree = {
    import c.mirror.universe._

    class InlineTerm(name:TermName, value:Tree) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(`name`) => value
        case _ => super.transform(tree)
      }
    }

    val termTypeMap = typeMap.map { case (name, tpe) => (name.toTermName:c.Name) -> Ident(tpe.typeSymbol.name.toTermName)}

    new Transformer() {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(x) if typeMap.contains(x) =>
          TypeTree(typeMap(x))
        case Ident(x) if termTypeMap.contains(x) =>
          termTypeMap(x)
        case Apply(aa@Ident(x), args) if valExpansions.contains(x) =>
          val (tname, tmap) = valExpansions(x)
          val mappedTree = tmap(typeMap(tname))
          mappedTree match {
            case fn@Function(fargs, body) =>
              (fargs zip args).foldLeft(body){ (currentBody, pair) =>
                val (fa, a) = pair
                new InlineTerm(fa.name, a).transform(currentBody)
              }
            case x => x
          }
        case Ident(x) if valExpansions.contains(x) =>
          val (tname, tmap) = valExpansions(x)
          tmap(typeMap(tname))
        case _ =>
          super.transform(tree)
      }
    } transform rhs
  }



  /** for a valdef with a [[breeze.macros.expand.sequence]] annotation, converts the sequence of associations to a Map */
  private def solveSequence(context: Context)(v: context.mirror.universe.ValDef, typeMappings: Map[context.Name, List[context.Type]]):(context.Name, Map[context.Type, context.Tree]) = {
    import context.mirror.universe._
    val x = v.mods.annotations.collectFirst{
      case x@q"new expand.sequence[${Ident(nme2)}](...$args)"  =>
        if (args.flatten.length != typeMappings(nme2).length) {
          context.error(x.pos, s"@sequence arguments list does not match the expand.args for $nme2")
        }
        val predef = context.mirror.staticModule("scala.Predef").asModule
        val missing = Select(Ident(predef), newTermName("???"))
        nme2 -> (typeMappings(nme2) zip args.flatten).toMap.withDefaultValue(missing)
    }
    x.get
  }

  /**
   * Returns the set of all types that this type should be unrolled as.
   * @param c
   * @param td
   * @return
   */
  private def typeMappings(c: Context)(td: c.mirror.universe.TypeDef):List[c.mirror.universe.Type] = {
    import c.mirror.universe._

    val mods = td.mods.annotations.collect{ case tree@q"new expand.args(...$args)" =>
      val flatArgs:Seq[Tree] = args.flatten
      flatArgs.map(c.typeCheck(_)).map{ tree =>
        try {
          tree.symbol.asModule.companionSymbol.asType.toType
        }  catch {
          case ex: Exception => c.abort(tree.pos, s"${tree.symbol} does not have a companion. Is it maybe an alias?")
        }
      }
    }.flatten
    mods
  }

  private def makeTypeMaps(c: Context)(types: ListMap[c.Name, Seq[c.Type]]):Seq[Map[c.Name, c.Type]] = {
    types.foldLeft(Seq(Map.empty[c.Name, c.Type])){ (acc, pair) =>
      val (nme, types) = pair
      for(t <- types; map <- acc) yield map + (nme -> t)
    }
  }

  private def getExclusions(c: Context)(mods: c.Modifiers, targs: Seq[c.Name]):Seq[Map[c.Name, c.Type]] = {
    import c.mirror.universe._
    mods.annotations.collect {
        case t@q"new expand.exclude(...$args)" =>
          for(aa <- args)
            if(aa.length != targs.length)
              c.error(t.pos, "arguments to @exclude does not have the same arity as the type symbols!")
          args.map(aa => (targs zip aa.map(c.typeCheck(_)).map(_.symbol.asModule.companionSymbol.asType.toType)).toMap)
    }.flatten.toSeq
  }

    private def checkValify(c: Context)(mods: c.Modifiers) = {
    import c.mirror.universe._
    mods.annotations.collectFirst {
        case q"new expand.valify" => true
    }.getOrElse(false)
  }

  private def shouldExpand(c: Context)(td: c.mirror.universe.TypeDef):Boolean = {
    import c.mirror.universe._
    td.mods.annotations.exists{
      case q"new expand.args(...$args)" => true
      case _ => false
    }
  }

  private def shouldExpandVarg(c: Context)(td: c.mirror.universe.ValDef):Boolean = {
    import c.mirror.universe._
    td.mods.annotations.exists{
      case x@q"new expand.sequence[..$targs](...$args)" => true
      case _ => false
    }
  }
}
