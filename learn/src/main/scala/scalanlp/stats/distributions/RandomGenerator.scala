package scalanlp.stats.distributions;
/*
Copyright 2009 David Hall, Daniel Ramage

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at 

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License. 
*/


/**
* Class for generating random numbers given
* the ability to generate a uniform int between
* MIN_INT and MAX_INT.
*
* This is designed to take the place of java.util.Random.
*
* @author dlwh
*/
trait RandomGenerator {
  def nextInt:Int

  def nextInt(n: Int):Int = {
    if (n<=0) throw new IllegalArgumentException("n must be positive");

    var result = 0;
    var bits = 0;
    do {
      bits = math.abs(nextInt)
      result = (bits) % n;
    } while(bits - result + (n-1) < 0);
    result
  }

  
  def nextDouble:Double = {
    // Taken from Cern
    val ret = (nextLong.toDouble - -9.223372036854776E18)  *  5.421010862427522E-20;
    if(ret > 0.0 && ret <1.0) ret
    else nextDouble
  }

  def nextLong = {
    // also from CERN:
    ((nextInt & 0xFFFFFFFFL) << 32) | ((nextInt & 0xFFFFFFFFL));
  }

  def nextFloat: Float = nextDouble.toFloat;
  def nextBoolean:Boolean = if((nextInt & 1) == 0) false else true;

  private var haveNextNextGaussian: Boolean = false;
  private var nextNextGaussian: Double = 0.0;

  def nextGaussian: Double  = synchronized {
    // Cribbed from Sun's javadocs
    if (haveNextNextGaussian) {
      haveNextNextGaussian = false;
      nextNextGaussian;
    } else {
      var v1 = 0.0;
      var v2 = 0.0;
      var s = 0.0;
      do { 
        v1 = 2 * nextDouble - 1;   // between -1.0 and 1.0
        v2 = 2 * nextDouble - 1;   // between -1.0 and 1.0
        s = v1 * v1 + v2 * v2;
      } while (s >= 1 || s == 0);
      val multiplier = math.sqrt(-2 * math.log(s)/s);
      nextNextGaussian = v2 * multiplier;
      haveNextNextGaussian = true;
      v1 * multiplier;
    }
  }


}
