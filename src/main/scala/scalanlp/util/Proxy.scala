package scalanlp.util;

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
