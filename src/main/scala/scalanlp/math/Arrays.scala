package scalanlp.math;

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
* Utilities and implicits for iterators. Nothing major.
* @author dlwh
*/
object Arrays {
  def normalize(arr :Array[Double]) = {
    val c = arr.foldLeft(0.0)(_+_);
    if(c == 0) arr;
    else arr.map( _ / c).force;
  }
}
