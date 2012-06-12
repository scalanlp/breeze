package breeze.numerics

/**
 *
 * @author dlwh
 */

object IntMath {
  def ipow(base: Int, exp: Int):Int = {
    var b = base
    if(exp < 0 && base != 1) 0
    else if(exp < 0) 1
    else {
      var e = exp
      var result = 1
      while (e != 0) {
        if ( (e & 1) != 0)
          result *= b
        e >>= 1
        b *= b
      }

      result
    }
  }
}
