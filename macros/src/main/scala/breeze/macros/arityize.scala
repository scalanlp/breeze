package breeze.macros

import scala.annotation.{StaticAnnotation, Annotation}
import scala.reflect.macros.Context

/**
 * TODO
 *
 * @author dlwh
 **/
class arityize extends Annotation with StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro arityize.arityizeImpl

}

object arityize {
  class replicate extends Annotation with StaticAnnotation
  class repeat extends Annotation with StaticAnnotation
  class relative(to: Any) extends Annotation with StaticAnnotation

  def arityizeImpl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.mirror.universe._
    annottees.head.tree match {
      case tree @ ClassDef(mods, name, targs, impl) =>
        val maxOrder: Int = extractOrder(c)

        val results = for (order <- 1 to maxOrder) yield {
          val bindings = Map(name.encoded -> order)
          val newTemplate =
            Template(impl.parents, impl.self, impl.body.flatMap(x => expandArity(c, order, bindings)(x)))
          val newTargs = targs.flatMap(arg => expandTypeDef(c, order, bindings)(arg))
          ClassDef(mods, newTypeName(name.encoded + order), newTargs, newTemplate)
        }

        val ret = c.Expr(Block(results.toList, Literal(Constant(()))))
        ret
      case tree @ DefDef(mods, name, targs, vargs, tpt, impl) =>
        val maxOrder: Int = extractOrder(c)

        val results = for (order <- 1 to maxOrder) yield {
          val bindings = Map(name.encoded -> order)

          val newImpl = expandArity(c, order, bindings)(impl).head
          val newVargs = vargs.map(_.flatMap(arg => expandValDef(c, order, bindings)(arg)))
          val newTargs = targs.flatMap(arg => expandTypeDef(c, order, bindings)(arg))
          val newRet = expandArity(c, order, bindings)(tpt).head
          DefDef(mods, newTermName(name.encoded + order), newTargs, newVargs, newRet, newImpl)
        }

        val ret = c.Expr(Block(results.toList, Literal(Constant(()))))
        ret
      case _ => ???
    }

  }

  def expandArity(c: Context, order: Int, bindings: Map[String, Int])(tree: c.Tree): Seq[c.Tree] = {
    import c.mirror.universe._

    tree match {
      case x @ DefDef(mods, name, targs, vargs, ret, impl) =>
        val newImpl = expandArity(c, order, bindings)(impl).head
        val newVargs = vargs.map(_.flatMap(arg => expandValDef(c, order, bindings)(arg)))
        val newTargs = targs.flatMap(arg => expandTypeDef(c, order, bindings)(arg))
//        println(name + " "+  ret + " " + newVargs)
        val newRet = expandArity(c, order, bindings)(ret).head
        Seq(DefDef(mods, name, newTargs, newVargs, newRet, newImpl))
      case x @ ClassDef(mods, name, targs, impl) =>
        val newParents = impl.parents.flatMap(tree => expandArity(c, order, bindings)(tree))
        val newTemplate = Template(newParents, impl.self, impl.body.flatMap(x => expandArity(c, order, bindings)(x)))
        val newTargs = targs.flatMap(arg => expandTypeDef(c, order, bindings)(arg))
        Seq(ClassDef(mods, name, newTargs, newTemplate))
      case vdef @ ValDef(mods, name, tpt, rhs) =>
        expandValDef(c, order, bindings)(vdef)
      case vdef @ TypeDef(mods, name, tpt, rhs) =>
        expandTypeDef(c, order, bindings)(vdef)
      case Annotated(ann, tree) =>
        ann match {
          case q"new arityize.relative($sym)" =>
            tree match {
              case Ident(nme) if nme.isTypeName => Seq(Ident(newTypeName(nme.encoded + bindings(sym.toString))))
              case Ident(nme) if nme.isTermName => Seq(Ident(newTermName(nme.encoded + bindings(sym.toString))))
              case AppliedTypeTree(Ident(nme), targs) =>
                val newName = Ident(newTypeName(nme.encoded + bindings(sym.toString)))
                val newTargs = targs.flatMap(arg => expandArity(c, order, bindings)(arg))
                Seq(AppliedTypeTree(newName, newTargs))
              case _ =>
//                println(tree + " " + tree.getClass); ???
                ???
            }
          case q"new arityize.replicate()" =>
            tree match {
              case Ident(nme) if nme.isTypeName =>
                List.tabulate(order) { i =>
                  Ident(newTypeName(nme.encoded + (i + 1)))
                }
              case Ident(nme) if nme.isTermName =>
                List.tabulate(order) { i =>
                  Ident(newTermName(nme.encoded + (i + 1)))
                }
              case _ => ???
            }
          case q"new arityize.repeat()" =>
            tree match {
              case Ident(nme) if nme.isTypeName =>
                List.fill(order) { tree }
              case Ident(nme) if nme.isTermName =>
                List.fill(order) { tree }
              case _ => ???
            }
          case _ =>
//            println("???" + ann + " " + tree)
            Seq(tree)
        }
      case Block(stats, ret) =>
        Seq(Block(stats.flatMap(st => expandArity(c, order, bindings)(st)), expandArity(c, order, bindings)(ret).last))
      case Ident(nme) if nme.encoded == "__order__" => Seq(Literal(Constant(order)))
      case t @ Ident(x) => Seq(t)
      case t @ Literal(x) => Seq(t)
      case Apply(who, args) =>
        for (w2 <- expandArity(c, order, bindings)(who);
          args2 = args.flatMap(arg => expandArity(c, order, bindings)(arg))) yield {
          Apply(w2, args2)
        }
      case Select(lhs, name) =>
        for (w2 <- expandArity(c, order, bindings)(lhs)) yield {
          Select(w2, name)
        }
      case AppliedTypeTree(lhs, targs) =>
        val newLHS = expandArity(c, order, bindings)(lhs).head

        val newTargs = targs.flatMap(arg => expandArity(c, order, bindings)(arg))
        Seq(AppliedTypeTree(newLHS, newTargs))
      case New(tree) =>
        Seq(New(expandArity(c, order, bindings)(tree).head))
      case _ =>
//        println("???" + tree + " " + tree.getClass)
        Seq(tree)
    }
  }

  def expandValDef(c: Context, order: Int, bindings: Map[String, Int])(
      vdef: c.universe.ValDef): List[c.universe.ValDef] = {
    import c.mirror.universe._
    if (shouldExpand(c)(vdef.mods)) {
      List.tabulate(order) { i =>
        val newBindings = bindings + (vdef.name.encoded -> (i + 1))
//        println(vdef.tpt + " " + expandArity(c, order, newBindings)(vdef.tpt).head)
        ValDef(
          vdef.mods,
          newTermName(vdef.name.encoded + (i + 1)),
          expandArity(c, order, newBindings)(vdef.tpt).head,
          vdef.rhs)
      }
    } else {
      shouldRelativize(c)(vdef.mods) match {
        case Some(x) =>
          val newBindings = bindings + (vdef.name.encoded -> bindings(x))
          val newTpt = expandArity(c, order, newBindings)(vdef.tpt).head
          List(ValDef(vdef.mods, newTermName(vdef.name.encoded + bindings(x)), newTpt, vdef.rhs))
        case _ =>
          val newTpt = expandArity(c, order, bindings)(vdef.tpt).head
          List(ValDef(vdef.mods, vdef.name, newTpt, vdef.rhs))
      }
    }

  }

  def expandTypeDef(c: Context, order: Int, bindings: Map[String, Int])(
      vdef: c.universe.TypeDef): List[c.universe.TypeDef] = {
    import c.mirror.universe._
    if (shouldExpand(c)(vdef.mods)) {
      List.tabulate(order)(i => TypeDef(vdef.mods, newTypeName(vdef.name.encoded + (i + 1)), vdef.tparams, vdef.rhs))
    } else if (shouldRepeat(c)(vdef.mods)) {
      List.fill(order)(vdef)
    } else {
      shouldRelativize(c)(vdef.mods) match {
        case Some(x) =>
          List(TypeDef(vdef.mods, newTypeName(vdef.name.encoded + bindings(x)), vdef.tparams, vdef.rhs))
        case _ =>
          List(vdef)
      }
    }

  }

  private def shouldExpand(c: Context)(td: c.mirror.universe.Modifiers): Boolean = {
    import c.mirror.universe._
    td.annotations.exists {
      case q"new arityize.replicate" => true
      case _ => false
    }
  }

  private def shouldRepeat(c: Context)(td: c.mirror.universe.Modifiers): Boolean = {
    import c.mirror.universe._
    td.annotations.exists {
      case q"new arityize.repeat" => true
      case _ => false
    }
  }

  private def shouldRelativize(c: Context)(td: c.mirror.universe.Modifiers): Option[String] = {
    import c.mirror.universe._
    td.annotations.collectFirst {
      case q"new arityize.relative($q)" => q.toString
    }
  }

  private def extractOrder(c: Context): Int = {
    import c.mirror.universe._
    val order = c.macroApplication.collect {
      case Literal(x) if x.value.isInstanceOf[Int] => x.value.toString.toInt
    }.head
    order
  }

}
