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


  def genBinaryOperator(name: String, typeA: String, typeB: String, op: OpType, result: String)(loop: String) = {
    """
  implicit val Name: BinaryOp[TypeA, TypeB, TypeOp, Result] = {
    new BinaryOp[TypeA, TypeB, TypeOp, Result] {
      def apply(a: TypeA, b: TypeB) = {
        LOOP
      }
    }
  }
""".replaceAll("TypeA",typeA).replaceAll("Name",name).replaceAll("Result",result).replaceAll("TypeB", typeB).replaceAll("TypeOp", op.getClass.getName.replaceAll("[$]","")).replaceAll("LOOP",loop)
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

  def binaryUpdateDV_V_loop(op: (String,String)=>String, zeroIsIdempotent: Boolean):String = {
    """require(b.length == a.length, "Vectors must be the same length!")

    for( (i,v) <- b.%s) {
      a(i) = %s
    }
    """.format(if(zeroIsIdempotent) "activeIterator" else "iterator", op("a(i)","v")).replaceAll("    ","        ")
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


  def binaryUpdateDM_M_loop(op: (String, String)=>String, ignored: Boolean):String = {
    """
      |        require(a.rows == b.rows, "Matrices must have same number of rows!")
      |        require(a.cols == b.cols, "Matrices must have same number of cols!")
      |        val ad = a.data
      |        var c = 0
      |        while(c < a.cols) {
      |          var r = 0
      |          while(r < a.rows) {
      |            ad(a.linearIndex(r, c)) = %s
      |            r += 1
      |          }
      |          c += 1
      |        }
    """.stripMargin.format(op("ad(a.linearIndex(r,c))","b(r,c)"))
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

  def genHomogeneous(tpe: String, generic: String, pckg: String, f: File)(loop: ((String,String)=>String)=>String,
                                                                         loopS: ((String,String)=>String)=>String,
                                                                         loopG: (((String,String)=>String),Boolean)=>String) {
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
      val gvector = "%s[%s]".format(generic,scalar)
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

        val namegen = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_V_"+scalar
        println(genBinaryUpdateOperator(namegen, vector, gvector, op)(loopG(fn, op == OpAdd || op == OpSub)))
        println()
        println("  " +genBinaryAdaptor(namegen.replace("Into",""), vector, gvector, op, vector, "pureFromUpdate_"+scalar+ "(" + namegen+ ")"))
        println()


      }
      println("}")
    }
    print.close()
  }

  val out = new File("math/src/main/scala/breeze/linalg/DenseVectorOps.scala")
  genHomogeneous("DenseVector", "Vector", "breeze.linalg", out)(
    GenOperators.binaryUpdateDV_DV_loop _,
    GenOperators.binaryUpdateDV_scalar_loop _,
    GenOperators.binaryUpdateDV_V_loop _
  )
  val outM = new File("math/src/main/scala/breeze/linalg/DenseMatrixOps.scala")
  genHomogeneous("DenseMatrix", "Matrix", "breeze.linalg", outM)(
    GenOperators.binaryUpdateDM_DM_loop _,
    GenOperators.binaryUpdateDM_scalar_loop _,
    GenOperators.binaryUpdateDM_M_loop _)
}

object GenDVSVSpecialOps extends App {
  import GenOperators._

  def fastLoop(op: (String,String)=>String):String = {
    """require(b.length == a.length, "Vectors must be the same length!")

    val bd = b.data
    val bi = b.index
    val bsize = b.iterableSize
    var i = 0
    while(i < bsize) {
      if(b.isActive(i)) a(bi(i)) = %s
      i += 1
    }
    """.format(op("a(bi(i))","bd(i)")).replaceAll("    ","        ")
  }


  def slowLoop(op: (String,String)=>String):String = {
    """require(b.length == a.length, "Vectors must be the same length!")

    var i = 0
    while(i < b.length) {
      a(i) = %s
      i += 1
    }
    """.format(op("a(i)","b(i)")).replaceAll("    ","        ")
  }

  def gen(sparseType: String, out: PrintStream) {
    import out._

    println("package breeze.linalg")
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    for( (scalar,ops) <- GenOperators.ops) {
      println()
      val vector = "%s[%s]".format("DenseVector",scalar)
      val svector = "%s[%s]".format(sparseType,scalar)
      println("/** This is an auto-generated trait providing operators for DenseVector and " + sparseType + "*/")
      println("trait DenseVectorOps_"+sparseType+"_"+scalar +" { this: DenseVector.type =>")
      for( (op,fn) <- ops) {
        val name = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_" + sparseType + "_" + scalar
        val loop = if(op == OpSub || op == OpAdd) fastLoop _ else slowLoop _

        println(genBinaryUpdateOperator(name, vector, svector, op)(loop(fn)))
        println()
        println("  " +genBinaryAdaptor(name.replace("Into",""), vector, svector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
        println()

      }



      // dot product
      val dotName = "canDotProductDV_SV_" + scalar
      println(genBinaryOperator(dotName, vector, svector, OpMulInner, scalar){
        """require(b.length == a.length, "Vectors must be the same length!")

       var result: """ + scalar + """ = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < b.size) {
          if(b.isActive(i)) result += a(bi(i)) * bd(i)
          i += 1
        }
        result""".replaceAll("       ","        ")
      })

      println("}")


    }
  }

  val out = new PrintStream(new FileOutputStream(new File("math/src/main/scala/breeze/linalg/DenseVectorSVOps.scala")))
  gen("SparseVector", out)
  out.close()

}

/*
object GenCounterOps extends App {
  import GenOperators._

  def plusSubIntoLoop(op: (String,String)=>String):String = {
    """
    for( (k,v) <- b.active.pairs) {
      a(k) = %s
    }
    """.format(op("a(k)","v")).replaceAll("    ","        ")
  }

  def timesLoop(op: (String,String)=>String):String = {
    """
    val zero = implicitly[Semiring[V]].zero
    val result = Counter[K, V]()
    for( (k,v) <- b.active.pairs) {
      val va =
      r(k) = %s
    }

    r
    """.format(op("a(k)","v")).replaceAll("    ","        ")
  }


  def slowLoop(op: (String,String)=>String):String = {
    """require(b.length == a.length, "Vectors must be the same length!")

    var i = 0
    while(i < b.length) {
      a(i) = %s
      i += 1
    }
    """.format(op("a(i)","b(i)")).replaceAll("    ","        ")
  }

  def gen(sparseType: String, out: PrintStream) {
    import out._

    println("package breeze.linalg")
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    for( (scalar,ops) <- GenOperators.ops) {
      println()
      val vector = "%s[%s]".format("DenseVector",scalar)
      val svector = "%s[%s]".format(sparseType,scalar)
      println("/** This is an auto-generated trait providing operators for DenseVector and " + sparseType + "*/")
      println("trait DenseVectorOps_"+sparseType+"_"+scalar +" { this: DenseVector.type =>")
      for( (op,fn) <- ops) {
        val name = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_" + sparseType + "_" + scalar
        val loop = if(op == OpSub || op == OpAdd) fastLoop _ else slowLoop _

        println(genBinaryUpdateOperator(name, vector, svector, op)(loop(fn)))
        println()
        println("  " +genBinaryAdaptor(name.replace("Into",""), vector, svector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
        println()

      }



      // dot product
      val dotName = "canDotProductDV_SV_" + scalar
      println(genBinaryOperator(dotName, vector, svector, OpMulInner, scalar){
        """require(b.length == a.length, "Vectors must be the same length!")

       var result: """ + scalar + """ = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < b.size) {
          if(b.isActive(i)) result += a(bi(i)) * bd(i)
          i += 1
        }
        result""".replaceAll("       ","        ")
      })

      println("}")


    }
  }

  val out = new PrintStream(new FileOutputStream(new File("math/src/main/scala/breeze/linalg/DenseVectorSVOps.scala")))
  gen("SparseVector", out)
  out.close()

}
*/

object GenAll extends App {
  GenDenseOps.main(Array.empty)
  GenDVSVSpecialOps.main(Array.empty)
}