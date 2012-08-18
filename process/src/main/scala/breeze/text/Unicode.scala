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
package breeze;
package text;

/**
 * Utilities for querying unicode code-points.  These supplement the
 * static methods in java.lang.Character;
 *
 * @author dramage
 */
object Unicode {
  private def inRanges(cp : Int, rangeStarts : Array[Int], rangeEnds : Array[Int]) : Boolean = {
    if (cp < 0) {
      return false;
    }
    
    var i = 0;
    while (i < rangeStarts.length && cp < rangeStarts(i)) {
      i += 1;
    }
    (i < rangeEnds.length) && (cp <= rangeEnds(i));
  }

  private val punctuationRangeStarts =
    Array(0xFF01,0xFF1A,0xFF3B,0xFF5B,0xFFE0,0xFE10,0xFE30,0x3000,0xFE50,0x2E00,0x0021,0x003A,0x005B,0x007B,0x2000,0x0080,0x00A1,0x00B4,0x00B6,0x00BF,0x00D7,0x00F7).sorted;

  private val punctuationRangeEnds =
    Array(0xFF0F,0xFF20,0xFF40,0xFF65,0xFFEE,0xFE1F,0xFE4F,0x303F,0xFE6F,0x2E7F,0x002F,0x003F,0x0060,0x007E,0x206F,0x00FF,0x00B1,0x00B4,0x00BB,0x00BF,0x00D7,0x00F7).sorted;

  /** Returns true if the given unicode code point is punctuation. */
  def isPunctuation(cp : Int) = {
    inRanges(cp, punctuationRangeStarts, punctuationRangeEnds)
  }

}
