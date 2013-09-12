package breeze.macros

import scala.reflect.macros.Context
import scala.language.experimental.macros
import scala.annotation.{Annotation, StaticAnnotation}

/**
 * TODO
 *
 * @author dlwh
 **/
class expand extends Annotation with StaticAnnotation {
  def macroTransform(annottees: Any*) = macro expand.expandImpl

}

class expandArgs(args: Any*) extends Annotation with StaticAnnotation
class sequence[T](args: Any*) extends Annotation with StaticAnnotation

object expand {



  def expandImpl(c: Context)(annottees: c.Expr[Any]*):c.Expr[Any] = {
    import c.mirror.universe._
    annottees.head.tree match {
      case tree@DefDef(mods, name, targs, vargs, tpt, rhs) =>

        val (typesToExpand, typesLeftAbstract) = targs.partition(shouldExpand(c)(_))

        val typesToUnrollAs = typesToExpand.map{ td =>
          (td.name:Name) -> typeMappings(c)(td)
        }.toMap

        val (valsToExpand, valsToLeave) = vargs.map(_.partition(shouldExpandVarg(c)(_))).unzip

        val valsToExpand2 = valsToExpand.flatten

        val configurations = makeTypeMaps(c)(typesToUnrollAs)
        val valExpansions = valsToExpand2.map{v => v.name -> solveSequence(c)(v, typesToUnrollAs)}.asInstanceOf[List[(c.Name, (c.Name, Map[c.Type, c.Tree]))]].toMap

        val newDefs = configurations.map{ typeMap =>
          val grounded = substitute(c)(typeMap, valExpansions, rhs)
          val newvargs = valsToLeave.filterNot(_.isEmpty).map(_.map(substitute(c)(typeMap, valExpansions, _).asInstanceOf[ValDef]))
          val newtpt = substitute(c)(typeMap, valExpansions, tpt)
          val newName = newTermName(name.toString + "_"+typeMap.map{ case (k,v) => k.toString +"_"+ v.toString.reverse.takeWhile(_ != '.').reverse}.mkString("_"))
          DefDef(mods, newName, typesLeftAbstract, newvargs, newtpt, grounded)
        }
        val ret = c.Expr(Block(newDefs.toList, Literal(Constant(()))))
        ret
      case _ => ???
    }
  }


  def substitute(c: Context)(typeMap: Map[c.Name, c.Type], valExpansions: Map[c.Name, (c.Name, Map[c.Type, c.Tree])], rhs: c.mirror.universe.Tree): c.mirror.universe.Tree = {
    import c.mirror.universe._

    class InlineTerm(name:TermName, value:Tree) extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(`name`) => value
        case _ => super.transform(tree)
      }
    }

    new Transformer() {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(x) if typeMap.contains(x) =>
          TypeTree(typeMap(x))
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



  /** for a valdef with a [[breeze.macros.sequence]] annotation, converts the sequence of associations to a Map */
  private def solveSequence(context: Context)(v: context.mirror.universe.ValDef, typeMappings: Map[context.Name, List[context.Type]]):(context.Name, Map[context.Type, context.Tree]) = {
    import context.mirror.universe._
    val x = v.mods.annotations.collectFirst{
      case x@q"new ${Ident(nme)}[${Ident(nme2)}](...$args)"  if (nme:Name).decoded == "sequence" =>
        nme2 -> (typeMappings(nme2) zip args.flatten).toMap
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

    val mods = td.mods.annotations.collect{ case q"new ${Ident(nme)}(...$args)" if (nme:Name).decoded == "expandArgs" =>
      val flatArgs:Seq[Tree] = args.flatten
      flatArgs.map(c.typeCheck(_)).map(_.symbol.asModule.companionSymbol.asType.toType)
    }.flatten
    mods
  }

  private def makeTypeMaps(c: Context)(types: Map[c.Name, Seq[c.Type]]):Seq[Map[c.Name, c.Type]] = {
    types.foldLeft(Seq(Map.empty[c.Name, c.Type])){ (acc, pair) =>
      val (nme, types) = pair
      for(t <- types; map <- acc) yield map + (nme -> t)
    }
  }

  private def shouldExpand(c: Context)(td: c.mirror.universe.TypeDef):Boolean = {
    import c.mirror.universe._
    td.mods.annotations.exists{
      case q"new ${Ident(nme)}(...$args)" if (nme:Name).decoded == "expandArgs" => true
      case _ => false
    }
  }

  private def shouldExpandVarg(c: Context)(td: c.mirror.universe.ValDef):Boolean = {
    import c.mirror.universe._
    td.mods.annotations.exists{
      case x@q"new ${Ident(nme)}[..$targs](...$args)" =>
        (nme:Name).decoded == "sequence"
      case _ => false
    }
  }
}