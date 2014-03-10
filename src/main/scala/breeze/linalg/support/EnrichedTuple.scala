package breeze.linalg.support.enrichedTuple

import breeze.math.Complex
import breeze.linalg.{DenseMatrix, DenseVector, max}
import breeze.macros.arityize

/**
 * @author ktakagaki
 * @date 3/8/14.
 */

  class EnrichedTupleBase {
    protected def valueCode( any: Any ): Int = {
      any match {
        case a: Int => 1
        case a: Long => 2
        case a: Float => 3
        case a: Double => 4
        case a: Complex => 5
// To be used for TupleNN.m()
//        case a: Tuple1[Any] => -1
//        case a: Tuple2[Any, Any] => -2
//        case a: Tuple3[Any, Any, Any] => -3
//        case a: Tuple4[Any, Any, Any, Any] => -4
//        case a: Tuple5[Any, Any, Any, Any, Any] => -5
//        case a: Tuple6[Any, Any, Any, Any, Any, Any] => -6
//        case a: Tuple7[Any, Any, Any, Any, Any, Any, Any] => -7
//        case a: Tuple8[Any, Any, Any, Any, Any, Any, Any, Any] => -8
//        case a: Tuple9[Any, Any, Any, Any, Any, Any, Any, Any, Any] => -9
//        case a: Tuple10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -10
//        case a: Tuple11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -11
//        case a: Tuple12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -12
//        case a: Tuple13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -13
//        case a: Tuple14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -14
//        case a: Tuple15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -15
//        case a: Tuple16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -16
//        case a: Tuple17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -17
//        case a: Tuple18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -18
//        case a: Tuple19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -19
//        case a: Tuple20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -20
//        case a: Tuple21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -21
//        case a: Tuple22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => -22
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

//  @arityize(22)
//  case class EnrichedTuple(tuple: (Tuple[Any @arityize.repeat] @arityize.relative(EnrichedTuple)) ) extends EnrichedTupleBase {
case class EnrichedTuple1( tuple: Tuple1[Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple2( tuple: Tuple2[Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple3( tuple: Tuple3[Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple4( tuple: Tuple4[Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple5( tuple: Tuple5[Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple6( tuple: Tuple6[Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple7( tuple: Tuple7[Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple8( tuple: Tuple8[Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple9( tuple: Tuple9[Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple10( tuple: Tuple10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple11( tuple: Tuple11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple12( tuple: Tuple12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple13( tuple: Tuple13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple14( tuple: Tuple14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple15( tuple: Tuple15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple16( tuple: Tuple16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple17( tuple: Tuple17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple18( tuple: Tuple18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple19( tuple: Tuple19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple20( tuple: Tuple20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple21( tuple: Tuple21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
case class EnrichedTuple22( tuple: Tuple22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] ) extends EnrichedTupleBase {
  def v() = {
    val temp = tuple.productIterator.map( valueCode(_) ).toArray
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
