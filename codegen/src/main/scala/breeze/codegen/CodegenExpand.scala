package breeze.codegen

import java.io.File
import java.nio.file.Files
import scala.collection.immutable.ListMap
import scala.meta._

object CodegenExpand {

  def codegenFile(inputFile: File, outputFile: File) = {
    val input = Input.File(inputFile)
    val exampleTree: Source = input.parse[Source].get
    val outputTree: Tree = processTree(exampleTree)

    outputFile.getParentFile.mkdirs()
    Files.write(
      outputFile.toPath,
      outputTree.syntax.getBytes("UTF-8")
    )
  }

  def processTree(tree: Tree): Tree = {
    flatTransformDefns(tree) {
      case Defn.Def(mods, name, targs, vargs, rtype, rhs) if hasExpand(mods) =>
        val (typesToExpand, typesLeftAbstract) = targs.partition(shouldExpand)

        val exclusions = getExclusions(mods, targs.map(_.name))
        val shouldValify = checkValify(mods)

        val typesToUnrollAs: ListMap[String, List[Type]] = ListMap.empty ++ typesToExpand.map { td =>
          td.name.value -> typeMappings(td)
        }

        val (valsToExpand, valsToLeave) = vargs.map(_.partition(shouldExpandVarg)).unzip
        val valsToExpand2 = valsToExpand.flatten

        val configurations = transposeListMap(typesToUnrollAs).filterNot(exclusions.toSet)
        val valExpansions = valsToExpand2.map { v =>
          v.name.value -> solveSequence(v, typesToUnrollAs)
        }.toMap

        val newDefs = configurations.map { typeMap =>
          val grounded = substitute(typeMap, valExpansions, rhs).asInstanceOf[Term]
          val newvargs =
            valsToLeave.filterNot(_.isEmpty).map(_.map(substitute(typeMap, valExpansions, _).asInstanceOf[Term.Param]))
          val newtpt = rtype.map(substitute(typeMap, valExpansions, _).asInstanceOf[Type])
          val newName = mkName(name, typesToExpand.map(t => typeMap(t.name.value).toString))
          val newMods = stripOurAnnotations(mods)
          if (shouldValify) {
            if (typesLeftAbstract.nonEmpty)
              error(tree.pos, "Can't valify: Not all types were grounded: " + typesLeftAbstract.mkString(", "))
            if (newvargs.exists(_.nonEmpty))
              error(
                tree.pos,
                "Can't valify: Not all arguments were grounded: " + newvargs
                  .map(_.mkString(", "))
                  .mkString("(", ")(", ")"))
            Defn.Val(
              mods = newMods,
              pats = List(Pat.Var(name = Term.Name(newName))),
              decltpe = newtpt,
              rhs = grounded
            )
          } else {
            val newTargs = typesLeftAbstract.map(substitute(typeMap, valExpansions, _)).asInstanceOf[List[Type.Param]]
            Defn.Def(newMods, Term.Name(newName), newTargs, newvargs, newtpt, grounded)
          }
        }

        newDefs

    }
  }

  private def hasExpand(mods: List[Mod]): Boolean = mods.exists{
    case Mod.Annot(Init(Type.Name("expand"), _, _)) => true
    case _ => false
  }

  private def error(pos: Position, message: String) = throw new RuntimeException(s"$pos $message")

  private def flatTransformDefns(tree: Tree)(pf: PartialFunction[Defn, Seq[Defn]]): Tree = {
    tree.transform {
      case cls: Defn.Class =>
        val newStats = cls.templ.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        cls.copy(templ=cls.templ.copy(stats=newStats))
      case cls: Defn.Trait =>
        val newStats = cls.templ.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        cls.copy(templ=cls.templ.copy(stats=newStats))
      case cls: Source =>
        val newStats = cls.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        cls.copy(stats=newStats)
      case obj: Defn.Object =>
        val newStats = obj.templ.stats.flatMap {
          case defn: Defn => pf.applyOrElse(defn, (_: Any) => Seq(defn))
          case x => Seq(x)
        }
        obj.copy(templ=obj.templ.copy(stats=newStats))
    }
  }

  private object ExType {
    def unapply(q: Type): Option[String] = q match {
      case n@Type.Select(_, _) => Some(n.syntax)
      case n@Type.Name(_) => Some(n.syntax)
      case _ => None

    }
  }

  /** for a valdef with a [[breeze.macros.expand.sequence]] annotation, converts the sequence of associations to a Map.
   * The return value is the name of the associated abstract type and the sequence of concrete values to sub in*/
  private def solveSequence(v: Term.Param, typeMappings: Map[String, List[Type]]): (String, Map[String, Term]) = {
    v.mods.collectFirst {
      case m@Mod.Annot(i@Init(Type.Apply(ExType("expand.sequence"), Seq(correspondingType)), _, args)) =>
        val name = coerceNameFromType(correspondingType)
        if (args.flatten.length != typeMappings(name).length) {
          error(m.pos, s"@sequence arguments list does not match the expand.args for name")
        }
        name -> typeMappings(name).zip(args.flatten).toMap.map { case (k, v) => coerceNameFromType(k) -> v}
    }.get
  }

  /**
   * Returns the set of all types that this type should be unrolled as.
   * @
   * param c
   * @param td
   * @return
   */
  private def typeMappings(td: Type.Param): List[Type] = {
    val mods = td.mods.collect {
      case Mod.Annot(Init(ExType("expand.args"), _, args)) =>
        args.flatten.map { tree => termNameToType(tree) }
    }.flatten
    mods
  }

  private def transposeListMap[A, B](types: ListMap[A, Seq[B]]): Seq[Map[A, B]] = {
    types.foldLeft(Seq(Map.empty[A, B])) { (acc, pair) =>
      val (nme, types) = pair
      for (t <- types; map <- acc) yield map + (nme -> t)
    }
  }

  private def getExclusions(mods: List[Mod], targs: Seq[Name]): Seq[Map[Name, Type]] = {
    mods.collect {
      case  t@Mod.Annot(i@Init(ExType("expand.exclude"), _, List(args))) =>
        if (args.length != targs.length)
          error(t.pos, "arguments to @exclude does not have the same arity as the type symbols!")
        targs.zip(args.map(aa => termNameToType(aa))).toMap
    }
  }

  private def checkValify(mods: List[Mod]) = {
    mods.collectFirst {
      case Mod.Annot(i@Init(ExType("expand.valify"), _, _)) => true
    }.getOrElse(false)
  }

  private def shouldExpand(td: Type.Param): Boolean = {
    td.mods.exists {
      case Mod.Annot(Init(ExType("expand.args"), _, args)) =>
        true
      case _ => false
    }
  }

  private def shouldExpandVarg(td: Term.Param): Boolean = {
    td.mods.exists {
      case Mod.Annot(i@Init(Type.Apply(ExType("expand.sequence"), targs), _, args)) => true
      case _ => false
    }
  }

  private def termNameToType(aa: Term): Type.Name = {
    Type.Name(aa.asInstanceOf[Term.Name].value)
  }

  private def mkName(name: Name, groundedTypes: Seq[String]): String = {
    groundedTypes.map { _.reverse.takeWhile(_ != '.').reverse }.mkString(name.toString + "_", "_", "")
  }

  private def substitute(typeMap: Map[String, Type], valExpansions: Map[String, (String, Map[String, Term])], body: Tree): Tree = {
    body.transform {
      case Type.Name(x) if typeMap.contains(x) =>
        typeMap(x)
      case Term.Name(x) if typeMap.contains(x) =>
        val nme = coerceNameFromType(typeMap(x))
        Term.Name(nme)
      case Term.Apply(Term.Name(x), args) if valExpansions.contains(x) =>
        val (tname, tmap) = valExpansions(x)
        val mappedTree = tmap(coerceNameFromType(typeMap(tname)))
        // TODO: this is super fragile. macro annotations handled this fairly well since scala had already
        // done the _ + _ --> (a$1, b$2) => a$1 + b$2 transform
        var i = 0
        mappedTree.transform {
          case x: Term.Placeholder =>
            i += 1
            args(i - 1)
          case fn@Term.Function(fargs, body) =>
            body.transform {
              case n@Term.Name(name) =>
                val pos = fargs.indexWhere(_.name.value == name)
                if (pos >= 0) {
                  args(pos)
                } else {
                  n
                }
            }
        }
      case Term.Name(x) if valExpansions.contains(x) =>
        val (tname, tmap) = valExpansions(x)
        tmap(coerceNameFromType(typeMap(tname)))
    }
  }

  private def coerceNameFromType(tpe: Type) = {
    tpe.asInstanceOf[Type.Name].value
  }

  private def stripOurAnnotations(mods: List[Mod]): List[Mod] = {
    mods.filter {
      case Mod.Annot(Init(ExType("expand"), _, args)) => false
      case Mod.Annot(Init(Type.Select(Term.Name("expand"), _), _, args)) => false
      case _ => true
    }
  }

  def main(args: Array[String]): Unit = {
    val x: Source = source"""
  class Foo {
  @expand
  @expand.valify
  implicit def dv_v_Op[
      @expand.args(Int, Double, Float, Long) T,
      @expand.args(OpAdd, OpSub, OpMulScalar, OpDiv, OpSet, OpMod, OpPow) Op <: OpType](
      implicit @expand.sequence[Op]({ _ + _ }, { _ - _ }, { _ * _ }, { _ / _ }, { (x, y) => y }, { _ % _ }, { _.pow(_) })
      op: Op.Impl2[T, T, T]): BinaryRegistry[DenseVector[T], Vector[T], Op.type, DenseVector[T]] =
    new BinaryRegistry[DenseVector[T], Vector[T], Op.type, DenseVector[T]] {

      override protected def bindingMissing(a: DenseVector[T], b: Vector[T]): DenseVector[T] = {
        val ad = a.data
        var aoff = a.offset
        val result = DenseVector.zeros[T](a.length)
        val rd = result.data

        var i = 0
        while (i < a.length) {
          rd(i) = op(ad(aoff), b(i))
          aoff += a.stride
          i += 1
        }
        result
      }
      implicitly[BinaryRegistry[Vector[T], Vector[T], Op.type, Vector[T]]].register(this)
    }
    }
          """

    val r = processTree(x)

    println(r)
  }
}