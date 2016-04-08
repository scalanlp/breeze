/*
 *
 *  Copyright 2015 David Hall
 *
 *  Licensed under the Apache License, Version 2.0 (the "License")
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 * /
 */

package breeze.linalg

/**
 * Provides extra aliases for [[breeze.linalg]], to maintain backward compatibility.
 *
 *
 * @author dlwh
 **/
package object support {

  /**
    * Type alias. This was originally a trait, but was refactored into [[breeze.generic.UFunc]] objects,
    * and this alias provides backward compatibility.
    *
    * @see [[breeze.linalg.mapValues]]
    */
  type CanMapValues[From, A, B, To] = mapValues.Impl2[From, A=>B, To]

  /**
    * Type alias. This was originally a trait, but was refactored into [[breeze.generic.UFunc]] objects,
    * and this alias provides backward compatibility.
    *
    * @see [[breeze.linalg.mapValues]]
    */
  type CanMapActiveValues[From, A, B, To] = mapActiveValues.Impl2[From, A=>B, To]

  val CanMapValues: mapValues.type = mapValues

}
