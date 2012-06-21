package breeze.generic

import collection.mutable.HashMap
import java.util.concurrent.ConcurrentHashMap
import breeze.util.ReflectionUtil

/**
 *
 * @author dlwh
 */

trait Multimethod[Method[AA, RR] <: MethodImpl[AA, RR], A <: AnyRef, R] extends MMRegistry1[Method[_ <: A, _ <: R]] { this: Method[A, R] =>
  protected def bindingMissing(a: A): R = throw new UnsupportedOperationException("Types not found!")
  protected def multipleOptions(a: A, m: Map[Class[_],Method[_ <: A, _ <: R]]) = {
    throw new RuntimeException("Multiple bindings for method: " + m)
  }

  def apply(a: A): R = {
    val ac = a.asInstanceOf[AnyRef].getClass

    val cached = cache.get(ac)
    if(cached != null) {
      cached.asInstanceOf[Method[A, R]].apply(a)
    } else {
      val options = resolve(ac)
      options.size match {
        case 0 => bindingMissing(a)
        case 1 =>
          val method = options.values.head
          cache.put(ac, method)
          method.asInstanceOf[Method[A, R]].apply(a)
        case _ =>
          val selected = selectBestOption(options)
          if(selected.size != 1)
            multipleOptions(a, options)
          else {
            val method = selected.values.head
            cache.put(ac, method)
            method.asInstanceOf[Method[A, R]].apply(a)
          }
      }
    }
  }


  def register[AA <: A](op: Method[AA, _ <: R])(implicit manA: Manifest[AA]) {
    super.register(manA.erasure.asInstanceOf[Class[AA]], op)
  }
}


/**
 * Basically Function1, but not because we don't want these coming up when implicit search happens...
 * @tparam A
 * @tparam R
 */
trait MethodImpl[A,+R] {
  def apply(a: A):R
}

trait Multimethod2[Method[AA,BB,RR]<:Function2[AA,BB,RR],A, B, R] extends ((A, B) => R) with MMRegistry2[Method[_<:A, _<:B, _<:R]] { this: Method[A, B, R] =>
  protected def bindingMissing(a: A, b: B):R = throw new UnsupportedOperationException("Types not found!" + a + b + " " + ops)
  protected def multipleOptions(a: A, b: B, m: Map[(Class[_],Class[_]),Method[_ <: A, _ <: B, _ <: R]]) = {
    throw new RuntimeException("Multiple bindings for method: " + m)
  }

  def apply(a: A, b: B): R = {
    val ac = a.asInstanceOf[AnyRef].getClass
    val bc = b.asInstanceOf[AnyRef].getClass

    val cached = cache.get(ac -> bc)
    if(cached != null) {
      cached match {
        case None => bindingMissing(a, b)
        case Some(m) =>
          m.asInstanceOf[Method[A, B, R]].apply(a, b)
      }
    } else {
      val options = resolve(ac, bc.asInstanceOf[Class[_<:B]])
      options.size match {
        case 0 =>
          cache.put(ac -> bc, None)
          bindingMissing(a, b)
        case 1 =>
          val method = options.values.head
          cache.put(ac -> bc, Some(method))
          method.asInstanceOf[Method[A, B, R]].apply(a, b)
        case _ =>
          val selected = selectBestOption(options)
          if(selected.size != 1)
            multipleOptions(a, b, options)
          else {
            val method = selected.values.head
            cache.put(ac -> bc, Some(method))
            method.asInstanceOf[Method[A, B, R]].apply(a, b)
          }
      }
    }
  }

  def register[AA<:A, BB<:B](op: Method[AA, BB, _ <: R])(implicit manA: Manifest[AA], manB: Manifest[BB]) {
    super.register(manA.erasure.asInstanceOf[Class[_]], manB.erasure.asInstanceOf[Class[_]], op)
  }

}


trait Multiproc2[Method[AA,BB]<:(AA, BB) => Unit,A<:AnyRef,B] extends ((A, B) => Unit) with MMRegistry2[Method[_<:A, _<:B]] { this: Method[A, B] =>
  protected def bindingMissing(a: A, b: B):Unit = throw new UnsupportedOperationException("Types not found!")
  protected def multipleOptions(a: A, b: B, m: Map[(Class[_],Class[_]),Method[_ <: A, _ <: B]]) = {
    throw new RuntimeException("Multiple bindings for method: " + m)
  }

  def apply(a: A, b: B):Unit = {
    val ac = a.asInstanceOf[AnyRef].getClass
    val bc = b.asInstanceOf[AnyRef].getClass

    val cached = cache.get(ac -> bc)
    if(cached != null) {
      cached match {
        case None => bindingMissing(a, b)
        case Some(m) =>
          m.asInstanceOf[Method[A, B]].apply(a, b)
      }
    } else {
      val m = resolve(a.getClass, b.asInstanceOf[AnyRef].getClass.asInstanceOf[Class[_<:B]])
      m.size match {
        case 0 => bindingMissing(a, b)
        case 1 =>
          val method = m.values.head
          cache.put(ac -> bc, Some(method))
          method.asInstanceOf[Method[A, B]].apply(a, b)
        case _ =>
          val selected = selectBestOption(m)
          if(selected.size != 1)
            multipleOptions(a, b, m)
          else {
            val method = selected.values.head
            cache.put(ac -> bc, Some(method))
            selected.values.head.asInstanceOf[Method[A, B]].apply(a, b)
          }
      }
    }
  }

  def register[AA<:A, BB<:B](op: Method[AA, BB])(implicit manA: Manifest[AA], manB: Manifest[BB]) {
    super.register(manA.erasure.asInstanceOf[Class[AA]], manB.erasure.asInstanceOf[Class[BB]], op)
  }

}


trait MMRegistry2[R] {
  protected val ops = HashMap[(Class[_],Class[_]), R]()
  protected val cache = new ConcurrentHashMap[(Class[_], Class[_]), Option[R]]()

  def register(a: Class[_], b: Class[_], op: R) {
    ops(a -> b) = op
    if(b.isPrimitive) {
      ops(a -> ReflectionUtil.boxedFromPrimitive(b)) = op
    }
    if(a.isPrimitive) {
      ops(ReflectionUtil.boxedFromPrimitive(a) -> b) = op
      if(b.isPrimitive) {
        ops(ReflectionUtil.boxedFromPrimitive(a) -> ReflectionUtil.boxedFromPrimitive(b)) = op
      }
    }
    cache.clear()
  }


  protected def resolve(a: Class[_], b: Class[_], checkedA: Set[Class[_]] = Set.empty, checkedB: Set[Class[_]] = Set.empty):Map[(Class[_], Class[_]), R] = {
    ops.get(a -> b) match {
      case Some(m) => Map((a->b) -> m)
      case None =>
        val newCA = checkedA ++ a.getInterfaces
        val newCB = checkedB ++ b.getInterfaces
        val sa = a.getSuperclass +: a.getInterfaces.filterNot(checkedA)
        val sb = b.getSuperclass +: b.getInterfaces.filterNot(checkedB)
        val oneParent = for(bb <- sb if bb != null; m <- resolve(a, bb, checkedA, newCB)) yield {
          m
        }
        val otherParent = for(aa <- sa if aa != null; m <- resolve(aa, b, newCA, checkedB)) yield {
          m
        }
        if(oneParent.isEmpty && otherParent.isEmpty) {
          val allParents = for(aa <- sa; bb <- sb if aa != null && bb != null; m <- resolve(aa, bb, newCA, newCB)) yield{
            m
          }
          allParents.toMap[(Class[_], Class[_]), R]
        } else {
          val rs = (oneParent ++ otherParent).toMap[(Class[_], Class[_]), R]
          rs
        }
    }

  }

  /** This selects based on the partial order induced by the inheritance hierarchy.
   *  If there is ambiguity, all are returned.
   **/
  protected def selectBestOption(options:Map[(Class[_],Class[_]),R]) = {
    var bestCandidates = Set[(Class[_],Class[_])]()
    for ( pair@(aa,bb) <- options.keys) {
      // if there is no option (aaa,bbb) s.t. aaa <: aa && bbb <: bb, then add it to the list
      if (!bestCandidates.exists(pair => aa.isAssignableFrom(pair._1) && bb.isAssignableFrom(pair._2))) {
        bestCandidates = bestCandidates.filterNot(pair => pair._1.isAssignableFrom(aa) && pair._2.isAssignableFrom(bb))
        bestCandidates += pair
      }
    }


    options.filterKeys(bestCandidates)
  }
}

trait MMRegistry1[M] {
  protected val ops = HashMap[Class[_], M]()
  protected val cache = new ConcurrentHashMap[Class[_], M]()

  def register(a: Class[_], op: M) {
    ops(a) = op
    if(a.isPrimitive) {
      ops(ReflectionUtil.boxedFromPrimitive(a)) = op
    }
    cache.clear()
  }


  protected def resolve(a: Class[_], checkedA: Set[Class[_]] = Set.empty):Map[Class[_], M] = {
    ops.get(a) match {
      case Some(m) => Map(a -> m)
      case None =>
        val newCA = checkedA ++ a.getInterfaces
        val sa = a.getSuperclass +: a.getInterfaces.filterNot(checkedA)
        val allParents = for(aa <- sa; if aa != null; m <- resolve(aa, newCA)) yield {
          m
        }
        allParents.toMap
    }

  }

  /** This selects based on the partial order induced by the inheritance hierarchy.
   *  If there is ambiguity, all are returned.
   **/
  protected def selectBestOption(options:Map[Class[_],M]) = {
    var bestCandidates = Set[Class[_]]()
    for ( aa <- options.keys) {
      // if there is no option (aaa,bbb) s.t. aaa <: aa && bbb <: bb, then add it to the list
      if (!bestCandidates.exists(c => aa.isAssignableFrom(c))) {
        bestCandidates = bestCandidates.filterNot(c => c.isAssignableFrom(aa))
        bestCandidates += aa
      }
    }


    options.filterKeys(bestCandidates)
  }
}