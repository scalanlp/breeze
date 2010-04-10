package scalanlp.stats.sampling;
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
* The Mersenne Twister is a random generator with a period of 2^19937-1,
* which is absurdly long. It's known to be fairly fast, too.
* 
* It needs a 32-bit integer seed.
*
* @author dlwh
*/
class MersenneTwister(seed: Int) extends RandomGenerator {
  /**
  * Seeds the generator with System.currentTimeMillis.toInt
  */
  def this() = this(System.currentTimeMillis.toInt);

  // Create a length 624 array to store the state of the generator
  private val MT = new Array[Int](624);
  private var index = 0
  MT(0) = seed
  for (i <- 1 to 623) { 
    MT(i) = (1812433253L * (MT(i-1) ^ (MT(i-1) >> 30)) + i)&0xffffffff toInt
  }

  // Extract a tempered pseudorandom number based on the index-th value,
  // calling generateNumbers() every 624 numbers
  def nextInt = {
    if (index == 0) {
      generateNumbers()
    }

    var y = MT(index);
    y = y ^ (y >>> 11);
    y = y ^ (( (y.toLong<<7) & (2636928640L))&0xffffffff) toInt;
    y = y ^ (( (y.toLong<<15) & (4022730752L))&0xffffffff) toInt;
    y = y ^ (y >>> 18);

    index = (index + 1) % 624
    y
  }

  // Generate an array of 624 untempered numbers
  private def generateNumbers() {
    for (i <- 0 to 623) {
      // 32'th bit of the one and the last 31 bits of the other
      val y = (MT(i) & 0x80000000) | (MT((i+1) % 624)&0x7fffffff);
      MT(i) = MT((i + 397) % 624) ^ (y >>> 1);
      if (y % 2 == 1) {
        MT(i) = MT(i) ^ (2567483615L) toInt;
      }
    }
  }
}
