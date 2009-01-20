package scalanlp.stats.sampling

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

import org.scalacheck._
import org.specs._;

import scalanlp.counters.Counters._;

object PitmanYorProcessSpec extends Specification with ScalaCheck {
  val arbPy = for {
    theta <- Gen.choose(0.0,20.0);
    alpha <- Gen.choose(0.0,1.0)
  } yield {
    new PitmanYorProcess(theta,alpha);
  }
  
  val arbPyWithDraw = for(py <- arbPy; n <- Gen.choose(0,5)) yield (py,n);
  
  val arbObsPy = for(py <- arbPy; list <- Gen.listOf(Gen.choose(0,5))) yield py.observe(count(list));
  val arbObsPyWithDraw = for(py <- arbObsPy; n <- Gen.choose(0,5)) yield (py,n);
  
  "observe increases probability" in {
    arbPyWithDraw must pass { pyn: (PitmanYorProcess,Int) =>
      val (py,n) = pyn;
      if(n == py.nextClass) true
      else py.probabilityOf(n) < py.observe(n).probabilityOf(n);
    }
  };
   
  "observe increases probability even after many observations" in {
    arbObsPyWithDraw must pass { pyn: (PitmanYorProcess,Int) =>
      val (py,n) = pyn;
      if(n == py.nextClass) true
      else py.probabilityOf(n) < py.observe(n).probabilityOf(n);
    }
  };
  
  "observe/unobserve has no effect" in {
    arbPyWithDraw must pass { pyn: (PitmanYorProcess,Int) =>
      val (py,n) = pyn;
      if(n == py.nextClass) true
      else py.probabilityOf(n) == py.observe(n).unobserve(n).probabilityOf(n);
    }
  }
  /*
  "observe/unobserve has no effect even after many observations" in {
    arbObsPyWithDraw must pass { pyn: (PitmanYorProcess,Int) =>
      val (py,n) = pyn;
      if(n == py.nextClass) true
      else {
        println();
        println(py);
        println(py.probabilityOf(n));
        println(py.observe(n).unobserve(n));
        println(py.observe(n).unobserve(n).probabilityOf(n));
        py.probabilityOf(n) == py.observe(n).unobserve(n).probabilityOf(n);
      }
    }
  }
   */
}

import org.specs.runner._;
class PitmanYorProcessTest extends JUnit4(PitmanYorProcessSpec);
