package breeze.linalg.support.tupleToDenseVector

import breeze.math.Complex
import breeze.linalg.{DenseVector, max}
import breeze.macros.arityize

/**
 * @author ktakagaki
 * @date 3/8/14.
 */

  class TupleToDenseVectorBase {
    protected def valueCode( any: Any ): Int = {
      //println(any.getClass.getName)
      any match {
        case a: Int => 1
        case a: Long => 2
        case a: Float => 3
        case a: Double => 4
        case a: Complex => 5
        case a: Any => 0
      }
    }

    protected def anyToInt( any: Any ): Int = any match {
      case a: Int => a
      case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Int")
    }

    protected def anyToLong( any: Any ): Long = any match {
      case a: Int => a.toLong
      case a: Long => a
      case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Long")
    }

    protected def anyToFloat( any: Any ): Float = any match {
      case a: Int => a.toFloat
      case a: Long => a.toFloat
      case a: Float => a
      case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Float")
    }

    protected def anyToDouble( any: Any ): Double = any match {
      case a: Int => a.toDouble
      case a: Long => a.toDouble
      case a: Float => a.toDouble
      case a: Double => a
      case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Double")
    }

    protected def anyToComplex( any: Any ): Complex = any match {
      case a: Int => Complex(a, 0d)
      case a: Long => Complex(a, 0d)
      case a: Float => Complex(a, 0d)
      case a: Double => Complex(a, 0d)
      case a: Complex => a
      case a: Any => throw new IllegalArgumentException("cannot widen type "+ a.getClass.getName +" to Complex")
    }
  }

  @arityize(22)
  @arityize.replicate
  class TupleToDenseVector(tuple: Tuple @arityize.relative(TupleToDenseVector) ) extends TupleToDenseVectorBase {
    def v() = {

      val temp = tuple.productIterator.map( valueCode(_) )

      if(temp.forall( _ > 0 )){
        max( temp ) match {
          case 1 => DenseVector( tuple.productIterator.map( anyToInt(_) ).toArray )
          case 2 => DenseVector( tuple.productIterator.map( anyToLong(_) ).toArray )
          case 3 => DenseVector( tuple.productIterator.map( anyToFloat(_) ).toArray )
          case 4 => DenseVector( tuple.productIterator.map( anyToDouble(_) ).toArray )
          case 5 => DenseVector( tuple.productIterator.map( anyToComplex(_) ).toArray )
        }
      } else {
        throw new IllegalArgumentException( "Cannot interpret tuples as DenseVector when they contain values with classes other than Int/Long/Float/Double/Complex" )
      }

    }


  }
