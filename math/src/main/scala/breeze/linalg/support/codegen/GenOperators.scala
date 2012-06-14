package breeze.linalg.support.codegen

import breeze.linalg.operators._
import java.io.{File, FileOutputStream, PrintStream}


/**
 *
 * @author dlwh
 */

object GenOperators {

  def genBinaryAdaptor(name: String, typeA: String, typeB: String, op: OpType, typeR: String, from: String) = (
    """implicit val %s: BinaryOp[%s, %s, %s, %s] = %s""".format(
      name,
      typeA,
      typeB,
      op.getClass.getName.replaceAll("[$]",""),
      typeR,
      from
    )


  )

  def genBinaryUpdateOperator(name: String, typeA: String, typeB: String, op: OpType)(loop: String) = {
    """
  implicit val Name: BinaryUpdateOp[TypeA, TypeB, TypeOp] = {
    new BinaryUpdateOp[TypeA, TypeB, TypeOp] {
      def apply(a: TypeA, b: TypeB) {
        LOOP
      }
    }
  }
""".replaceAll("TypeA",typeA).replaceAll("Name",name).replaceAll("TypeB", typeB).replaceAll("TypeOp", op.getClass.getName.replaceAll("[$]","")).replaceAll("LOOP",loop)
  }

  def binaryUpdateDV_scalar_loop(op: (String,String)=>String):String = {
    """val ad = a.data

    var i = 0
    var aoff = a.offset
    while(i < a.length) {
      ad(aoff) = %s
      aoff += a.stride
      i += 1
    }
    """.format(op("ad(aoff)","b")).replaceAll("    ","        ")
  }

  def binaryUpdateDV_DV_loop(op: (String,String)=>String):String = {
    """require(b.length == a.length, "Vectors must be the same length!")

    val ad = a.data
    val bd = b.data
    var aoff = a.offset
    var boff = b.offset

    var i = 0
    while(i < a.length) {
      ad(aoff) = %s
      aoff += a.stride
      boff += b.stride
      i += 1
    }
    """.format(op("ad(aoff)","bd(boff)")).replaceAll("    ","        ")
  }


  val ops = Map(
    "Double" -> Map[OpType,(String,String)=>String](OpAdd -> {_ + " + " + _},
      OpSub -> {_ + " - " + _},
      OpMulScalar -> {_ + " % " + _},
      OpDiv -> {_ + " / " + _},
      OpMod -> {_ + " % " + _},
      OpPow -> {"scala.math.pow("+ _ + ", " + _ + ")"}
    ),

    "Float" -> Map[OpType,(String,String)=>String](OpAdd -> {_ + " + " + _},
      OpSub -> {_ + " - " + _},
      OpMulScalar -> {_ + " % " + _},
      OpDiv -> {_ + " / " + _},
      OpMod -> {_ + " % " + _},
      OpPow -> {"scala.math.pow("+ _ + ", " + _ + ").toFloat"}
    ),

    "Int" -> Map[OpType,(String,String)=>String](OpAdd -> {_ + " + " + _},
        OpSub -> {_ + " - " + _},
        OpMulScalar -> {_ + " % " + _},
        OpDiv -> {_ + " / " + _},
        OpMod -> {_ + " % " + _},
        OpPow -> {"IntMath.ipow("+ _ + ", " + _ + ")"}
      )

  )
}

object GenDVOps extends App {

  def gen(pckg: String, f: File) {
    val out = new FileOutputStream(f)
    val print = new PrintStream(out)
    import print.println

    println("package " + pckg)
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    import GenOperators._
    for( (scalar,ops) <- GenOperators.ops) {
      val vector = "DenseVector[%s]" format scalar
      println("/** This is an auto-generated trait providing operators for DenseVectors. */")
      println("trait DenseVectorOps_"+scalar +" { this: DenseVector.type =>")

      println(
        """
  def pureFromUpdate_%s[Other,Op<:OpType](op: BinaryUpdateOp[%s, Other, Op])(implicit copy: CanCopy[%s]):BinaryOp[%s, Other, Op, %s] = {
    new BinaryOp[%s, Other, Op, %s] {
      override def apply(a : %s, b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        """.format(scalar,vector, vector, vector, vector, vector, vector, vector))

      for( (op,fn) <- ops) {
        val names = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_S_"+scalar
        println(genBinaryUpdateOperator(names, vector, scalar, op)(binaryUpdateDV_scalar_loop(fn)))
        println()
        val name = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_DV_"+scalar
        println(genBinaryUpdateOperator(name, vector, vector, op)(binaryUpdateDV_DV_loop(fn)))
        println()
        println("  " +genBinaryAdaptor(name.replace("Into",""), vector, vector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))


      }
      println("}")
    }
    print.close()
  }



  val out = new File("math/src/main/scala/breeze/linalg/DenseVectorOps.scala")
  gen("breeze.linalg", out)
}
