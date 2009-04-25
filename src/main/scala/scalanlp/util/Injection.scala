package scalanlp.util;

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
* Any function that can be unapplied is an injection.
*
* See Index for an example.
*
* @author dlwh
*/
trait Injection[A,B] extends Function1[A,B] { outer =>
  def apply(x: A): B;
  def unapply(x: B): Option[A];

  def andThen[C](f: Injection[B,C]):Injection[A,C] = new Injection[A,C] {
    def apply(x:A) = f(outer(x));
    def unapply(x:C) = f.unapply(x).flatMap(outer unapply _);
  }
}
