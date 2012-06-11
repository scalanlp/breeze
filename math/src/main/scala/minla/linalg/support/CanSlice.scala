package minla.linalg.support

/**
 *
 * @author dlwh
 */
trait CanSlice[-From, -Slice, +To] extends ((From, Slice)=>To)

/**
 *
 * @author dlwh
 */
trait CanSlice2[-From, -Slice1, -Slice2, +To] extends ((From,Slice1,Slice2)=>To)
