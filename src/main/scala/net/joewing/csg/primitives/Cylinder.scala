package net.joewing.csg.primitives

import net.joewing.csg.{BSPTree, Facet, Vertex}

case class Cylinder(length: Double, r1: Double, r2: Double, sides: Int) extends Solid {
  require(sides > 2, s"Cylinder must have at least 3 sides, got $sides")

  private val angle = 2.0 * math.Pi / sides

  private def renderEnd(r: Double, z: Double): Seq[Facet] = {
    if (r > 0) {
      (0 until sides).map { i =>
        val theta1 = angle * i
        val theta2 = angle * (i + 1)
        Facet(
          Vertex(0, 0, z),
          Vertex(r * math.cos(theta1), r * math.sin(theta1), z),
          Vertex(r * math.cos(theta2), r * math.sin(theta2), z)
        )
      }.toSeq
    } else {
      Seq.empty
    }
  }

  def render: BSPTree = {
    val top = renderEnd(r1, 0).map(_.flip)
    val bottom = renderEnd(r2, length)

    // 2 triangles per side
    val shaft = Range(0, sides).flatMap { i =>
      val theta1 = angle * i
      val theta2 = angle * (i + 1)
      val x1a = r1 * math.cos(theta1)
      val x2a = r1 * math.cos(theta2)
      val y1a = r1 * math.sin(theta1)
      val y2a = r1 * math.sin(theta2)
      val x1b = r2 * math.cos(theta1)
      val x2b = r2 * math.cos(theta2)
      val y1b = r2 * math.sin(theta1)
      val y2b = r2 * math.sin(theta2)
      val v1 = Vertex(x2a, y2a, 0)
      val v2 = Vertex(x1a, y1a, 0)
      val v3 = Vertex(x2b, y2b, length)
      val v4 = v3
      val v5 = v2
      val v6 = Vertex(x1b, y1b, length)
      val f1 = Facet(v3, v2, v1)
      val f2 = Facet(v6, v5, v4)
      if (r1 == 0) {
        Seq(f2)
      } else if (r2 == 0) {
        Seq(f1)
      } else {
        Seq(f1, f2)
      }
    }

    BSPTree(top ++ shaft ++ bottom)
  }
}
