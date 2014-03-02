package breeze.linalg.support

import breeze.math.Complex
import breeze.linalg.{max, DenseVector}

/**
 * @author ktakagaki
 * @date 3/1/14.
 */
class PimpedTuple( tuple: Product ) {
  def v() = {
    tupleOrValueCode(tuple) match {
      case 0 => throw new IllegalArgumentException("This object cannot be converted to DenseVector: " + tuple )
      case code if(code < 0) =>{
        val temp = tuple.productIterator.map( tupleOrValueCode(_) ).toList
        if(temp.forall( _ > 0 )){
          max( temp ) match {
            case 1 => DenseVector( tuple.productIterator.map( anyToInt(_) ).toArray )
            case 2 => DenseVector( tuple.productIterator.map( anyToLong(_) ).toArray )
            case 3 => DenseVector( tuple.productIterator.map( anyToFloat(_) ).toArray )
            case 4 => DenseVector( tuple.productIterator.map( anyToDouble(_) ).toArray )
            case 5 => DenseVector( tuple.productIterator.map( anyToComplex(_) ).toArray )
          }
        } else if(temp.forall( _ < 0 )){
          throw new IllegalArgumentException("Cannot create DenseMatrix's yet" )
        } else if(temp.reduce(_*_)==0){
          throw new IllegalArgumentException( "Cannot interpret tuples with non-tuple or non-Int/Long/Float/Double/Complex values" )
        }
      }

    }

  }


  private def tupleOrValueCode( any: Any ): Int = {
    //println(any.getClass.getName)
    any match {
      case a: Int => 1
      case a: Long => 2
      case a: Float => 3
      case a: Double => 4
      case a: Complex => 5
      case Tuple1(_) => -1
      case Tuple2(_, _) => -2
      case Tuple3(_, _, _) => -3
      case Tuple4(_, _, _, _) => -4
      case Tuple5(_, _, _, _, _) => -5
      case Tuple6(_, _, _, _, _, _) => -6
      case a: Any => 0
    }
  }

  private def anyToInt( any: Any ): Int = any match {
    case a: Int => a
    case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Int")
  }

  private def anyToLong( any: Any ): Long = any match {
    case a: Int => a.toLong
    case a: Long => a
    case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Long")
  }

  private def anyToFloat( any: Any ): Float = any match {
    case a: Int => a.toFloat
    case a: Long => a.toFloat
    case a: Float => a
    case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Float")
  }

  private def anyToDouble( any: Any ): Double = any match {
    case a: Int => a.toDouble
    case a: Long => a.toDouble
    case a: Float => a.toDouble
    case a: Double => a
    case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Double")
  }

  private def anyToComplex( any: Any ): Complex = any match {
    case a: Int => Complex(a, 0d)
    case a: Long => Complex(a, 0d)
    case a: Float => Complex(a, 0d)
    case a: Double => Complex(a, 0d)
    case a: Complex => a
    case a: Any => throw new IllegalArgumentException("cannot promote type "+ a.getClass.getName +" to Complex")
  }

}
