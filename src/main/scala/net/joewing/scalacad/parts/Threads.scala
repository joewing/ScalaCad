package net.joewing.scalacad.parts

import net.joewing.scalacad.primitives._

object Threads {
  /**
    * Create screw threads.
    * @param radius The outer radius.
    * @param length The length.
    * @param turns The number of screw turns.
    * @param threadRatio The thread:radius ratio (to determine the depth of the threads).
    * @param resolution The resolution (lower values add detail).
    * @return A 3d primitive representing screw threads.
    */
  def apply(
    radius: Double,
    length: Double,
    turns: Double = 1,
    threadRatio: Double = 0.75,
    resolution: Double = 0.2
  ): Primitive[ThreeDimensional] = {
    val slices = (length / resolution).toInt
    val sides = (radius / resolution).toInt
    val radiansPerSlice = math.Pi * turns / slices
    circle(radius, sides).scale(x = threadRatio).extrude(length, radiansPerSlice, slices)
  }
}
