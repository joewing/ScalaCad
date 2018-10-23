package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Facet, Vertex}

object Cylinder {
  def apply(
    length: Double,
    topRadius: Double,
    bottomRadiusOverride: Double = -1,
    sidesOverride: Int = 0
  ): Primitive3d = {

    val bottomRadius: Double = if (bottomRadiusOverride >= 0) bottomRadiusOverride else topRadius

    val fa = 12.0
    val fs = 2.0
    val sides: Int = if (sidesOverride > 0) math.max(sidesOverride, 3) else {
      val maxRadius = math.max(topRadius, bottomRadius)
      math.ceil(math.max(math.min(360.0 / fa, maxRadius * 2.0 * math.Pi / fs), 5)).toInt
    }

    val angle = 2.0 * math.Pi / sides

    def renderEnd(r: Double, z: Double): Vector[Facet] = {
      if (r > 0) {
        val vertices = Vector.range(0, sides).map { i =>
          val theta1 = angle * i
          val theta2 = angle * (i + 1)
          Vertex(r * math.cos(theta1), r * math.sin(theta1), z)
        }
        Facet.fromVertices(vertices)
      } else {
        Vector.empty
      }
    }

    val top = renderEnd(topRadius, 0).map(_.flip)
    val bottom = renderEnd(bottomRadius, length)

    // 1 square per side.
    val shaft = Vector.tabulate[Seq[Vertex]](sides) { i =>
      val theta1 = angle * i
      val theta2 = angle * (i + 1)
      val x1a = topRadius * math.cos(theta1)
      val x2a = topRadius * math.cos(theta2)
      val y1a = topRadius * math.sin(theta1)
      val y2a = topRadius * math.sin(theta2)
      val x1b = bottomRadius * math.cos(theta1)
      val x2b = bottomRadius * math.cos(theta2)
      val y1b = bottomRadius * math.sin(theta1)
      val y2b = bottomRadius * math.sin(theta2)
      val v1 = Vertex(x2a, y2a, 0)
      val v2 = Vertex(x1a, y1a, 0)
      val v3 = Vertex(x2b, y2b, length)
      val v4 = Vertex(x1b, y1b, length)
      if (topRadius == 0) {
        Seq(v4, v2, v3)
      } else if (bottomRadius == 0) {
        Seq(v3, v2, v1)
      } else {
        Seq(v3, v4, v2, v1)
      }
    }
    Primitive3d(top ++ shaft.flatMap(Facet.fromVertices) ++ bottom)
  }
}
