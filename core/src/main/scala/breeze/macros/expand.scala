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

object expand {
  def expandImpl(c: Context)(annottees: c.Expr[Any]*):c.Expr[Any] = {
    import c.mirror.universe._
    annottees.head.tree match {
      case tree@DefDef(mods, name, targs, vargs, tpt, rhs) =>

        val (typesToExpand, typesLeftAbstract) = targs.partition(shouldExpand(c)(_))
        println(typesToExpand, typesLeftAbstract)

        val typesToUnrollAs = typesToExpand.map{ td =>
          (td.name:Name) -> typeMappings(c)(td)
        }.toMap

      val configurations = makeConfigurations(c)(typesToUnrollAs)
      println(typesToUnrollAs)
        val newDefs = configurations.map{ typeMap =>
          val grounded = substitute(c)(typeMap, rhs)
          val newtpt = substitute(c)(typeMap, tpt)
          val newName = newTermName(name.toString + "_"+typeMap.map{ case (k,v) => k.toString +"_"+ v.toString}.mkString("_"))
          DefDef(mods, newName, typesLeftAbstract, vargs, newtpt, grounded)
        }
        println(newDefs)
        val ret = c.Expr(Block(newDefs.toList, Literal(Constant(()))))
      println(ret)
        ret
      case _ => ???
    }
  }


  def substitute(c: Context)(typeMap: Map[c.Name, c.Type], rhs: c.mirror.universe.Tree): c.mirror.universe.Tree = {
    import c.mirror.universe._
    new Transformer() {
      override def transform(tree: Tree): Tree = tree match {
        case Ident(x) if typeMap.contains(x) =>
          TypeTree(typeMap(x))
        case _ =>
          super.transform(tree)
      }
    } transform rhs
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

  private def makeConfigurations(c: Context)(types: Map[c.Name, Seq[c.Type]]):Seq[Map[c.Name, c.Type]] = {
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
}