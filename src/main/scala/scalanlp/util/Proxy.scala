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


import java.lang.reflect._;
import scala.Array;

object Proxy {
  def apply[T<:AnyRef,R](real:T)(replacement: AnyRef) :R = {
    val handler = new InvocationHandler {
      override def invoke(proxy: Any, m:Method, args: Array[Object]):Object = {
        tryInvoke(replacement,m,args) match {
          case None => m.invoke(real,args:_*);
          case Some(r)=> r.asInstanceOf[Object];
        }
      }

      private def tryInvoke(other: AnyRef, m: Method, args:Array[AnyRef]):Any = {
        val newM = try {
          Some(other.getClass.getMethod(m.getName,m.getParameterTypes:_*))
        } catch {
          case _ => None;
        }
        newM.map(_.invoke(other,args:_*));
      }
    }
    java.lang.reflect.Proxy.newProxyInstance(real.getClass.getClassLoader,real.getClass.getInterfaces,handler).asInstanceOf[R];
  }
}
