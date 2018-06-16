package net.joewing.csg.projection

import net.joewing.csg.Vertex

case class PerspectiveProjection(
  camera: Vertex,
  display: Vertex,
  theta: Vertex
) extends Projection {
  def project(v: Vertex): (Double, Double) = {
    val x = v.x1 - camera.x1
    val y = v.x2 - camera.x2
    val z = v.x3 - camera.x3

    val sx = math.sin(theta.x1)
    val sy = math.sin(theta.x2)
    val sz = math.sin(theta.x3)
    val cx = math.cos(theta.x1)
    val cy = math.cos(theta.x2)
    val cz = math.cos(theta.x3)

    val dx = cy * (sz * y + cz * x) - sy * z
    val dy = sx * (cy * z + sy * (sz * y + cz * x)) + cx * (cz * y - sz * x)
    val dz = cx * (cy * z + sy * (sz * y + cz * x)) - sx * (cz * y - sz * x)

    val bx = (display.x3 * dx) / dz - display.x1
    val by = (display.x3 * dy) / dz - display.x2

    (bx, by)
  }
}
