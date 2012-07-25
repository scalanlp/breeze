package breeze.linalg.support.codegen
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

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


  def genBinaryRegistryAdaptor(name: String, typeA: String, typeB: String, op: OpType, typeR: String, from: String) = (
    """implicit val %s: BinaryRegistry[%s, %s, %s, %s] = %s""".format(
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
  class Name private[linalg] () extends BinaryUpdateOp[TypeA, TypeB, TypeOp] {
    def apply(a: TypeA, b: TypeB) {
      LOOP
    }
  }; implicit val Name = new Name ()
""".replaceAll("TypeA",typeA).replaceAll("Name",name).replaceAll("TypeB", typeB).replaceAll("TypeOp", op.getClass.getName.replaceAll("[$]","")).replaceAll("LOOP",loop)
  }


  def genBinaryOperator(name: String, typeA: String, typeB: String, op: OpType, result: String)(loop: String) = {
    """
   class Name private[linalg] () extends BinaryOp[TypeA, TypeB, TypeOp, Result] {
    def apply(a: TypeA, b: TypeB) = {
      LOOP
    }
  }; implicit val Name = new Name ()
""".replaceAll("TypeA",typeA).replaceAll("Name",name).replaceAll("Result",result).replaceAll("TypeB", typeB).replaceAll("TypeOp", op.getClass.getName.replaceAll("[$]","")).replaceAll("LOOP",loop)
  }


  def genBinaryUpdateRegistry(name: String, typeA: String, typeB: String, op: OpType)(loop: String) = {
    """
  class Name private[linalg] () extends BinaryUpdateRegistry[TypeA, TypeB, TypeOp] {
    override def bindingMissing(a: TypeA, b: TypeB) {
      LOOP
    }
  }; implicit val Name = new Name ()
""".replaceAll("TypeA",typeA).replaceAll("Name",name).replaceAll("TypeB", typeB).replaceAll("TypeOp", op.getClass.getName.replaceAll("[$]","")).replaceAll("LOOP",loop)
  }


  def genBinaryRegistry(name: String, typeA: String, typeB: String, op: OpType, result: String)(loop: String) = {
    """
  class Name private[linalg] () extends BinaryRegistry[TypeA, TypeB, TypeOp, Result] {
    override def bindingMissing(a: TypeA, b: TypeB) = {
      LOOP
    }
  }; implicit val Name = new Name()
""".replaceAll("TypeA",typeA).replaceAll("Name",name).replaceAll("Result",result).replaceAll("TypeB", typeB).replaceAll("TypeOp", op.getClass.getName.replaceAll("[$]","")).replaceAll("LOOP",loop)
  }


  def register(owner: String, genName: String, myName: String) = {
    "%s.%s.register(%s)".format(owner, genName, myName)
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

  def binaryUpdateV_S_loop(op: (String,String)=>String, zeroIsIdempotent: Boolean):String = {
    """
    for( (i,v) <- a.%s) {
      a(i) = %s
    }
    """.format(if(zeroIsIdempotent) "activeIterator" else "iterator", op("v","b")).replaceAll("    ","        ")
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

  val blacklist = Map("DenseVector" -> Set("canMulScalarInto_DV_S_Double", "canSetInto_DV_DV_Double",
    "canAddInto_DV_DV_Double", "canSubInto_DV_DV_Double")).withDefaultValue(Set.empty)

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
        if(!blacklist(tpe)(name)) {
          println(genBinaryUpdateOperator(name, vector, vector, op)(loop(fn)))
          if(generic == "Vector")
            println("  " + register(generic, GenVectorRegistries.getVVIntoName(op, scalar), name))
          println()
          println("  " +genBinaryAdaptor(name.replace("Into",""), vector, vector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
          if(generic == "Vector")
            println("  " + register(generic, GenVectorRegistries.getVVName(op, scalar), name.replace("Into", "")))
          println()
        }
        val names = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_S_"+scalar
        if(!blacklist(tpe)(names)) {
          println(genBinaryUpdateOperator(names, vector, scalar, op)(loopS(fn)))
          if(generic == "Vector")
            println("  " + register(generic, GenVectorRegistries.getVSIntoName(op, scalar), names))
          println()
          println("  " +genBinaryAdaptor(names.replace("Into",""), vector, scalar, op, vector, "pureFromUpdate_"+scalar+ "(" + names+ ")"))
          if(generic == "Vector")
            println("  " + register(generic, GenVectorRegistries.getVSName(op, scalar), names.replaceAll("Into","")))
          println()
        }

        val namegen = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_DV_V_"+scalar
        println(genBinaryUpdateOperator(namegen, vector, gvector, op)(loopG(fn, op == OpAdd || op == OpSub)))
        if(generic == "Vector")
          println("  " + register(generic, GenVectorRegistries.getVVIntoName(op, scalar), namegen))
        println()
        println("  " +genBinaryAdaptor(namegen.replace("Into",""), vector, gvector, op, vector, "pureFromUpdate_"+scalar+ "(" + namegen+ ")"))
        if(generic == "Vector")
          println("  " + register(generic, GenVectorRegistries.getVVName(op, scalar), namegen.replace("Into", "")))
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
        println("  " + register("Vector", GenVectorRegistries.getVVIntoName(op, scalar), name))
        println()
        println("  " +genBinaryAdaptor(name.replace("Into",""), vector, svector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
        println("  " + register("Vector", GenVectorRegistries.getVVName(op, scalar), name.replace("Into","")))
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
        while(i < bsize) {
          if(b.isActive(i)) result += a(bi(i)) * bd(i)
          i += 1
        }
        result""".replaceAll("       ","        ")
      })
      println("  " + register("Vector", GenVectorRegistries.getDotName(scalar), dotName))

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

object GenSVOps extends App {
  import GenOperators._

  def plusIntoLoop(tpe: String, op: (String,String)=>String, postProcessCopy: String=>String):String = {
    """require(b.length == a.length, "Vectors must be the same length!")

    // TODO: decide the appropriate value of 3 and 30 here.
    if(b.activeSize > a.activeSize * 3 && b.activeSize > 30) {
      val c = copy(b)
      apply(c, a)
      %s
      a.use(c.index, c.data, c.activeSize)
      return
    }

    var buf:Array[Type] = null
    var bufi:Array[Int] = null
    var nactiveSize = 0

    val bd = b.data
    val bi = b.index
    val bsize = b.iterableSize
    var i = 0
    while(i < bsize) {
      if (a.contains(bi(i))) {
        // just add it in if it's there
        a(bi(i)) = %s
      } else { // not there
        if(buf eq null) {
          buf = new Array[Type](b.activeSize - i)
          bufi = new Array[Int](b.activeSize - i)
        } else if(buf.length == nactiveSize) {
          buf = Arrays.copyOf(buf, nactiveSize + b.activeSize - i)
          bufi = Arrays.copyOf(bufi, nactiveSize + b.activeSize - i)
        }

        // append to buffer to merged in later
        buf(nactiveSize) = %s
        bufi(nactiveSize) = bi(i)
        nactiveSize += 1
      }
      i += 1
    }

    // merge two disjoint sorted lists
    if(buf != null) {
      val result = new Array[Type](a.activeSize + nactiveSize)
      val resultI = new Array[Int](a.activeSize + nactiveSize)
      var ni = 0
      var ai = 0
      var out = 0

      while(ni < nactiveSize) {
        while(ai < a.activeSize && a.index(ai) < bufi(ni) ) {
          result(out) = a.data(ai)
          resultI(out) = a.index(ai)
          ai += 1
          out += 1
        }
        result(out) = buf(ni)
        resultI(out) = bufi(ni)
        out += 1
        ni += 1
      }

      System.arraycopy(a.data, ai, result, out, result.length - out)
      System.arraycopy(a.index, ai, resultI, out, result.length - out)
      out = result.length

      a.use(resultI, result, out)
    }
    """.replaceAll("Type",tpe).format(postProcessCopy("c"),op("a(bi(i))","bd(i)"), op("buf(nactiveSize)","bd(i)")).replaceAll("    ","        ")
  }

  def timesLoopTemplate(tpe: String, zero: String, finish: (String, String, String)=>String) = {
    """require(b.length == a.length, "Vectors must be the same length!")

    val outD = new Array[Type](a.activeSize min b.activeSize)
    val outI = new Array[Int](a.activeSize min b.activeSize)
    var out = 0

    val looper = if(a.activeSize < b.activeSize) a else b
    val other = if(a.activeSize < b.activeSize) b else a

    var i = 0
    val bd = looper.data
    val bi = looper.index
    val bsize = looper.iterableSize
    while(i < bsize) {
      if(looper.isActive(i)) {
        val p = other(bi(i)) * bd(i)
        if (p != Zero) {
          outD(out) = p
          outI(out) = bi(i)
          out += 1
        }
      }
      i += 1
    }

    %s
    """.replaceAll("Type",tpe).replaceAll("Zero", zero).format(finish("outD", "outI", "out"))
  }

  def timesIntoLoop(tpe: String, zero: String) = timesLoopTemplate(tpe, zero, {(data, index, activeSize) =>
    "a.use(%s, %s, %s)".format(index, data, activeSize)
  })


  def timesLoop(tpe: String, zero: String) = timesLoopTemplate(tpe, zero, {(data, index, activeSize) =>
    "new SparseVector(%s, %s, %s, a.length)".format(index, data, activeSize)
  })

  def setLoop = """require(b.length == a.length, "Vectors must be the same length!")
      a.use(Arrays.copyOf(b.index), Arrays.copyOf(b.data), b.activeSize)"""

  def slowLoop(op: (String,String)=>String):String = {
                """require(b.length == a.length, "Vectors must be the same length!")

    var i = 0
    while(i < b.length) {
      a(i) = %s
      i += 1
    }
    """.format(op("a(i)","b(i)")).replaceAll("    ","        ")
  }


  def scalarLoop(op: (String,String)=>String):String = {
                """

    var i = 0
    while(i < a.length) {
      a(i) = %s
      i += 1
    }
    """.format(op("a(i)","b")).replaceAll("    ","        ")
  }


  def scalarMultLoop(op: (String,String)=>String) = {
                """
    var i = 0
    while(i < a.activeSize) {
      a.data(i) = %s
      i += 1
    }
    """.format(op("a.data(i)","b"))
  }

  def gen(out: PrintStream) {
    import out._

    println("package breeze.linalg")
    println("import java.util._")
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    for( (scalar,ops) <- GenOperators.ops) {
      println()
      val vector = "SparseVector[%s]" format scalar
      println("/** This is an auto-generated trait providing operators for SparseVector */")
      println("trait SparseVectorOps_"+scalar +" { this: SparseVector.type =>")


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


      for( (op,fn) <- ops if op != OpMulScalar) {
        val name = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_VV_" + scalar
        def postProcesscopy(c: String) = if(op == OpAdd) "" else if(op == OpSub) c + "*= (-1).to"+scalar else sys.error(":(")
        val loop = if(op == OpSub || op == OpAdd) plusIntoLoop(scalar, (_:(String,String)=>String), postProcesscopy _) else slowLoop _

        println(genBinaryUpdateOperator(name, vector, vector, op)(loop(fn)))
        println("  " + register("Vector", GenVectorRegistries.getVVIntoName(op, scalar), name))
        println()
        println("  " +genBinaryAdaptor(name.replace("Into",""), vector, vector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
        println("  " + register("Vector", GenVectorRegistries.getVVName(op, scalar), name.replace("Into", "")))
        println()


        val names = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_SV_S_"+scalar
        val loopS = if(op == OpDiv) scalarMultLoop(fn) else scalarLoop(fn)
        println(genBinaryUpdateOperator(names, vector, scalar, op)(loopS))
        println("  " + register("Vector", GenVectorRegistries.getVSIntoName(op, scalar), names))
        println()
        println("  " +genBinaryAdaptor(names.replace("Into",""), vector, scalar, op, vector, "pureFromUpdate_"+scalar+ "(" + names+ ")"))
        println("  " + register("Vector", GenVectorRegistries.getVSName(op, scalar), names.replace("Into", "")))
        println()

      };

      { //mul
        val op = OpMulScalar
        val name = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_VV_" + scalar
        val loop = timesIntoLoop(scalar, "0")

        println(genBinaryUpdateOperator(name, vector, vector, op)(loop))
        println()

        val nonupdate = name.replace("Into","")
        println(genBinaryOperator(nonupdate, vector, vector, OpMulScalar, vector){timesLoop(scalar, "0")})
        println()

        val names = "can"+op.getClass.getSimpleName.drop(2).dropRight(1)+"Into_SV_S_"+scalar
        println(genBinaryUpdateOperator(names, vector, scalar, op)(scalarMultLoop(ops(OpMulScalar))))
        println()
        println("  " +genBinaryAdaptor(names.replace("Into",""), vector, scalar, op, vector, "pureFromUpdate_"+scalar+ "(" + names+ ")"))
        println()
      }



      // dot product
      val dotName = "canDotProductSV_" + scalar
      println(genBinaryOperator(dotName, vector, vector, OpMulInner, scalar){
        """require(b.length == a.length, "Vectors must be the same length!")

       var result: """ + scalar + """ = 0

        val bd = b.data
        val bi = b.index
        val bsize = b.iterableSize
        var i = 0
        while(i < bsize) {
          if(b.isActive(i)) result += a(bi(i)) * bd(i)
          i += 1
        }
        result""".replaceAll("       ","        ")
      })

      println("}")


    }
  }

  val out = new PrintStream(new FileOutputStream(new File("math/src/main/scala/breeze/linalg/SparseVectorOps.scala")))
  gen(out)
  out.close()

}


object GenVectorRegistries extends App {

  def genHomogeneous(tpe: String, pckg: String, f: File)(loop: (((String,String)=>String), Boolean)=>String,
                                                         loopS: (((String,String)=>String), Boolean)=>String) {
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
  def pureFromUpdate_%s[Other,Op<:OpType](op: BinaryUpdateOp[%s, Other, Op])(implicit copy: CanCopy[%s]):BinaryRegistry[%s, Other, Op, %s] = {
    new BinaryRegistry[%s, Other, Op, %s] {
      override def bindingMissing(a : %s, b : Other) = {
        val c = copy(a)
        op(c, b)
        c
      }
    }
  }
        """.format(scalar,vector, vector, vector, vector, vector, vector, vector))

      for( (op,fn) <- ops) {
        val name = getVVIntoName(op, scalar)
        println(genBinaryUpdateRegistry(name, vector, vector, op)(loop(fn, op == OpAdd || op == OpSub)))
        println()
        println("  " +genBinaryRegistryAdaptor(getVVName(op, scalar), vector, vector, op, vector, "pureFromUpdate_"+scalar+ "(" + name+ ")"))
        println()
        val names: String = getVSIntoName(op, scalar)
        println(genBinaryUpdateRegistry(names, vector, scalar, op)(loopS(fn, op == OpAdd || op == OpSub)))
        println()
        println("  " +genBinaryRegistryAdaptor(getVSName(op, scalar), vector, scalar, op, vector, "pureFromUpdate_"+scalar+ "(" + names+ ")"))
        println()


      }


      val dotName: String = getDotName(scalar)
      println(genBinaryRegistry(dotName, vector, vector, OpMulInner, scalar){
        """require(b.length == a.length, "Vectors must be the same length!")

       var result: """ + scalar + """ = 0

        for( (i, v) <- b.activeIterator) {
          result += a(i) * v
        }
        result""".replaceAll("       ","        ")
      })

      println("}")
    }
    print.close()
  }


  def getDotName(scalar: String): String = {
    val dotName = "canDotProductV_" + scalar
    dotName
  }

  def getVSIntoName(op: OpType, scalar: String): String = {
    val names = "can" + op.getClass.getSimpleName.drop(2).dropRight(1) + "Into_V_S_" + scalar
    names
  }

  def getVVIntoName(op: OpType, scalar: String): String = {
    "can" + op.getClass.getSimpleName.drop(2).dropRight(1) + "Into_V_V_" + scalar
  }


  def getVSName(op: OpType, scalar: String): String = {
    getVSIntoName(op, scalar).replaceAll("Into", "")
  }

  def getVVName(op: OpType, scalar: String): String = {
    getVVIntoName(op, scalar).replaceAll("Into", "")
  }

  val out = new File("math/src/main/scala/breeze/linalg/VectorOps.scala")
  genHomogeneous("Vector", "breeze.linalg", out)(
    GenOperators.binaryUpdateDV_V_loop _,
    GenOperators.binaryUpdateV_S_loop _
  )

}

object GenCSCOps extends App {
  import GenOperators._
  def gen(out: PrintStream) {
    import out._

    println("package breeze.linalg")
    println("import java.util._")
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    for( (scalar,ops) <- GenOperators.ops) {
      println()
      val matrix = "CSCMatrix[%s]" format scalar
      val gvector = "Vector[%s]" format scalar
      println("/** This is an auto-generated trait providing operators for CSCMatrix */")
      println("trait CSCMatrixOps_"+scalar +" { this: CSCMatrix.type =>")

      println(genBinaryOperator("canMulM_V_" + scalar, matrix, gvector, OpMulMatrix, gvector){"""
      val res = DenseVector.zeros[Scalar](a.rows)
      var c = 0
      while(c < a.cols) {
        var rr = a.colPtrs(c)
        val rrlast = a.colPtrs(c+1)
        while (rr < rrlast) {
          val r = a.rowIndices(rr)
          res(r) += a.data(rr) * b(c)
          rr += 1
        }
        c += 1
      }

      res                                                               """.replaceAll("Scalar", scalar)
      })


      println("}")

    }
  }

  val out = new PrintStream(new FileOutputStream(new File("math/src/main/scala/breeze/linalg/CSCMatrixOps.scala")))
  gen(out)
  out.close()

}

object GenDMMultOps extends App {
  import GenOperators._
  def gen(out: PrintStream) {
    import out._

    println("package breeze.linalg")
    println("import java.util._")
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    for( (scalar,ops) <- GenOperators.ops) {
      println()
      val matrix = "DenseMatrix[%s]" format scalar
      val vector = "DenseVector[%s]" format scalar
      val gvector = "Vector[%s]" format scalar
      val gmatrix = "Matrix[%s]" format scalar
      println("/** This is an auto-generated trait providing multiplication for DenseMatrix */")
      println("trait DenseMatrixMultOps_"+scalar +" extends DenseMatrixOps_" + scalar + " { this: DenseMatrix.type =>")

      println(genBinaryRegistry("canMulM_V_" + scalar, matrix, gvector, OpMulMatrix, vector){"""
      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[Scalar](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res                                                               """.replaceAll("Scalar", scalar)
      })

      println(genBinaryRegistry("canMulM_M_" + scalar, matrix, gmatrix, OpMulMatrix, matrix){"""
      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[Scalar](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res                                                               """.replaceAll("Scalar", scalar)
      })


      println("}")

    }
  }

  val out = new PrintStream(new FileOutputStream(new File("math/src/main/scala/breeze/linalg/DenseMatrixMulOps.scala")))
  gen(out)
  out.close()

}

object GenMatrixMultOps extends App {
  import GenOperators._
  def gen(out: PrintStream) {
    import out._

    println("package breeze.linalg")
    println("import java.util._")
    println("import breeze.linalg.operators._")
    println("import breeze.linalg.support._")
    println("import breeze.numerics._")

    for( (scalar,ops) <- GenOperators.ops) {
      println()
      val matrix = "Matrix[%s]" format scalar
      val vector = "Vector[%s]" format scalar
      val gvector = "Vector[%s]" format scalar
      val gmatrix = "Matrix[%s]" format scalar
      println("/** This is an auto-generated trait providing multiplication for Matrix */")
      println("trait MatrixMultOps_"+scalar +" { this: Matrix.type =>")

      println(genBinaryRegistry("canMulM_V_" + scalar, matrix, gvector, OpMulMatrix, vector){"""
      // TODO: this could probably be much faster?
      require(a.cols == b.length)
      val res = DenseVector.zeros[Scalar](a.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          res(r) += v * b(c)
          r += 1
        }
        c += 1
      }

      res                                                               """.replaceAll("Scalar", scalar)
      })

      println(genBinaryRegistry("canMulM_M_" + scalar, matrix, gmatrix, OpMulMatrix, matrix){"""
      // TODO: this could probably be much faster
      val res = DenseMatrix.zeros[Scalar](a.rows, b.cols)
      require(a.cols == b.rows)
      var c = 0
      while(c < a.cols) {
        var r = 0
        while (r < a.rows) {
          val v = a(r, c)
          var j = 0
          while(j < b.cols) {
            res(r, j) += v * b(c, j)
            j += 1
          }
          r += 1
        }
        c += 1
      }

      res                                                               """.replaceAll("Scalar", scalar)
      })


      println("}")

    }
  }

  val out = new PrintStream(new FileOutputStream(new File("math/src/main/scala/breeze/linalg/MatrixMulOps.scala")))
  gen(out)
  out.close()

}


object GenAll extends App {
  GenDenseOps.main(Array.empty)
  GenDVSVSpecialOps.main(Array.empty)
  GenSVOps.main(Array.empty)
  GenVectorRegistries.main(Array.empty)
  GenCSCOps.main(Array.empty)
  GenDMMultOps.main(Array.empty)
  GenMatrixMultOps.main(Array.empty)
}