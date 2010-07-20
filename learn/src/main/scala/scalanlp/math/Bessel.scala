package scalanlp.math

/*
Copyright 2010 David Hall, Daniel Ramage

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

import math._;
import Numerics._;


/**
 * Implementations of the Bessel functions, based on Numerical Recipes
 *
 * @author dlwh
 */
object Bessel {
  private val i0p = Array(9.999999999999997e-1,2.466405579426905e-1, 1.478980363444585e-2,
                          3.826993559940360e-4, 5.395676869878828e-6, 4.700912200921704e-8,
                          2.733894920915608e-10,1.115830108455192e-12, 3.301093025084127e-15,
                          7.209167098020555e-18,1.166898488777214e-20, 1.378948246502109e-23,
                          1.124884061857506e-26,5.498556929587117e-30);
  private val i0q = Array(4.463598170691436e-1,1.702205745042606e-3, 2.792125684538934e-6,
                          2.369902034785866e-9,8.965900179621208e-13);
  private val i0pp = Array(1.192273748120670e-1,1.947452015979746e-1, 7.629241821600588e-2,
                           8.474903580801549e-3,2.023821945835647e-4);
  private val i0qq = Array(2.962898424533095e-1,4.866115913196384e-1, 1.938352806477617e-1,
                           2.261671093400046e-2,6.450448095075585e-4, 1.529835782400450e-6);

  def i0(x: Double) = {
    val ax = x.abs;
    if (ax < 15.0) {
      val y = x*x;
      poly(i0p,y)/poly(i0q,225.-y);
    } else {	
      val z= 1.0-15.0/ax;
       exp(ax)*poly(i0pp,z)/(poly(i0qq,z)*sqrt(ax));
    }
  }
}
