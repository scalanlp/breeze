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
* Utilities for doing math on arrays
* @author dlwh
*/
object Arrays {
  def normalize(arr :Array[Double]) = {
    val c = arr.foldLeft(0.0)(_+_);
    if(c == 0) arr;
    else arr.map( _ / c).force;
  }

  def dotProduct(x: Array[Double], y: Array[Double]) = {
    require(x.length == y.length);
    var i = 0;
    var v = 0.0;
    while(i < x.length) {
      v += x(i) * y(i);
      i += 1;
    }
    v;
  }

  def scaleAdd(x: Array[Double], scale: Double, y: Array[Double]):Array[Double] = {
    scaleAdd(x,scale,y,new Array[Double](x.length));
  }

  def scaleAdd(x: Array[Double], scale: Double, y: Array[Double], sink: Array[Double]):Array[Double] = {
    require(x.length == y.length);
    require(x.length == sink.length);
    var i = 0;
    while(i < x.length) {
      sink(i) = x(i) + scale * y(i);
      i += 1; 
    }
    sink
  }
}
