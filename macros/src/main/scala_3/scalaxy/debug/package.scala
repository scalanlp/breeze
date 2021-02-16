package scalaxy

import scala.language.dynamics
import scala.language.experimental.macros

package object debug {
  inline def assert(inline condition: Boolean, inline message: Any): Unit = 
    ${ impl.assertImpl('condition, 'message)}
  
  transparent inline def assert(inline condition: Boolean): Unit =
    ${ impl.assertImpl('condition, null)}

  inline def require(inline condition: Boolean, inline message: Any): Unit =
    ${ impl.requireImpl('condition, 'message)}

  transparent inline def require(inline condition: Boolean): Unit =
    ${ impl.requireImpl('condition, null)}

  inline def assume(inline condition: Boolean, inline message: Any): Unit =
    ${ impl.assumeImpl('condition, 'message)}

  transparent inline def assume(inline condition: Boolean): Unit =
    ${ impl.assumeImpl('condition, null)}
}
