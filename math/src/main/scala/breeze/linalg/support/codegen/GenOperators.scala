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

  def binaryUpdateDM_scalar_loop(op: (String, String)=>String):String = {
    """
    val ad = a.data
    var c = 0
    while(c < a.cols) {
       var r = 0
       while(r < a.rows) {
         ad(a.linearIndex(r, c)) = %s
         r += 1
       }
       c += 1
    }
    """.format(op("ad(a.linearIndex(r,c))","b"))
  }

  def binaryUpdateDM_DM_loop(op: (String, String)=>String):String = {
    """
      |        require(a.rows == b.rows, "Matrices must have same number of rows!")
      |        require(a.cols == b.cols, "Matrices must have same number of cols!")
      |        val ad = a.data
      |        val bd = b.data
      |        var c = 0
      |        while(c < a.cols) {
      |          var r = 0
      |          while(r < a.rows) {
      |            ad(a.linearIndex(r, c)) = %s
      |            r += 1
      |          }
      |          c += 1
      |        }
    """.stripMargin.format(op("ad(a.linearIndex(r,c))","bd(b.linearIndex(r,c))"))
  }

  val ops = Map(
    "Double" -> Map[OpType,(String,String)=>String](OpAdd -> {_ + " + " + _},
      OpSub -> {_ + " - " + _},
      OpMulScalar -> {_ + " * " + _},
      OpDiv -> {_ + " / " + _},
      OpMod -> {_ + " % " + _},
      OpSet -> {(a,b) => b},
      OpPow -> {"scala.math.pow("+ _ + ", " + _ + ")"}
    ),

    "Float" -> Map[OpType,(String,String)=>String](OpAdd -> {_ + " + " + _},
      OpSub -> {_ + " - " + _},
      OpMulScalar -> {_ + " * " + _},
      OpDiv -> {_ + " / " + _},
      OpMod -> {_ + " % " + _},
      OpSet -> {(a,b) => b},
      OpPow -> {"scala.math.pow("+ _ + ", " + _ + ").toFloat"}
    ),

    "Int" -> Map[OpType,(String,String)=>String](OpAdd -> {_ + " + " + _},
        OpSub -> {_ + " - " + _},
        OpMulScalar -> {_ + " * " + _},
        OpDiv -> {_ + " / " + _},
        OpMod -> {_ + " % " + _},
        OpSet -> {(a,b) => b},
        OpPow -> {"IntMath.ipow("+ _ + ", " + _ + ")"}
      )

  )
}

object GenDenseOps extends App {

  def gen(tpe: String, pckg: String, f: File)(loop: ((String,String)=>String)=>String, loopS: ((String,String)=>String)=>String) {
    val out = new FileOutputStream(f)
    val print = new PrintStream(out)
    import print.println

    println("package " + pckg)
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    import GenOperators._
    for( (scalar,ops) <- GenOperators.ops) {
      val vector = "%s[%s]".format(tpe,scalar)
      println("/** This is an auto-generated trait providing operators for " + tpe + ". */")
      println("trait "+tpe+"Ops_"+scalar +" { this: "+tpe+".type =>")

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
        val name = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_DV_"+scalar
        println(genBinaryUpdateOperator(name, vector, vector, op)(loop(fn)))
        println()
        println("  " +genBinaryAdaptor(name.replace("Into",""), vector, vector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
        println()
        val names = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_S_"+scalar
        println(genBinaryUpdateOperator(names, vector, scalar, op)(loopS(fn)))
        println()
        println("  " +genBinaryAdaptor(names.replace("Into",""), vector, scalar, op, vector, "pureFromUpdate_"+scalar+ "(" + names+ ")"))
        println()


      }
      println("}")
    }
    print.close()
  }



  val out = new File("math/src/main/scala/breeze/linalg/DenseVectorOps.scala")
  gen("DenseVector","breeze.linalg", out)(GenOperators.binaryUpdateDV_DV_loop _, GenOperators.binaryUpdateDV_scalar_loop _)
  val outM = new File("math/src/main/scala/breeze/linalg/DenseMatrixOps.scala")
  gen("DenseMatrix","breeze.linalg", outM)(GenOperators.binaryUpdateDM_DM_loop _, GenOperators.binaryUpdateDM_scalar_loop _)
}
