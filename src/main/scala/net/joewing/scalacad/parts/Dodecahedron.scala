package net.joewing.scalacad.parts

import net.joewing.scalacad.primitives._

object Dodecahedron {
  def apply(size: Double): Primitive[ThreeDimensional] = {
    val obj = (0 to 4).foldLeft(cube(2, 2, 1).centered) { (a, i) =>
      a & cube(2, 2, 1).centered.rotate(x = math.toRadians(116.565)).rotate(z = math.toRadians(72 * i))
    }
    obj.scale(size, size, size)
  }
}
