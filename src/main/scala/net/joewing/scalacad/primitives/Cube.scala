package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Polygon3d, Vertex}

object Cube {
  def apply(width: Double, height: Double, depth: Double): Primitive3d = {
    val sides = Vector(
      Array(Vertex(0, 0, 0), Vertex(0, height, 0), Vertex(width, height, 0), Vertex(width, 0, 0)),
      Array(Vertex(width, height, depth), Vertex(0, height, depth), Vertex(0, 0, depth), Vertex(width, 0, depth)),
      Array(Vertex(width, 0, depth), Vertex(0, 0, depth), Vertex(0, 0, 0), Vertex(width, 0, 0)),
      Array(Vertex(0, height, 0), Vertex(0, height, depth), Vertex(width, height, depth), Vertex(width, height, 0)),
      Array(Vertex(0, height, depth), Vertex(0, height, 0), Vertex(0, 0, 0), Vertex(0, 0, depth)),
      Array(Vertex(width, 0, depth), Vertex(width, 0, 0), Vertex(width, height, 0), Vertex(width, height, depth))
    )
    Primitive3d(sides.map(Polygon3d.apply))
  }
}
