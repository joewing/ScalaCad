package net.joewing.csg.parts

import net.joewing.csg.BSPTree
import net.joewing.csg.primitives._

case class Dodecahedron(size: Double) extends Primitive[ThreeDimensional] {
  private lazy val obj = (0 to 4).foldLeft(cube(2, 2, 1).centered) { (a, i) =>
    a & cube(2, 2, 1).centered.rotate(x = math.toRadians(116.565)).rotate(z = math.toRadians(72 * i))
  }

  def render: BSPTree = obj.render
}
