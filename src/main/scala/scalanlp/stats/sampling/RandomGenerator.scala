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
* Class for generating random numbers given
* the ability to generate a uniform int between
* MIN_INT and MAX_INT
*/
trait RandomGenerator {
  def nextInt:Int
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
  
}
