package net.joewing.scalacad.primitives

import net.joewing.scalacad.{Polygon3d, RenderedObject, Vertex}

import scala.concurrent.{ExecutionContext, Future}

final case class Transform[D <: Dim](obj: Primitive[D], func: Vertex => Vertex) extends Primitive[D] {
  val dim: D = obj.dim

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    obj.renderedFuture.map(_.map(p => Polygon3d(p.vertices.map(func))))
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] = {
    obj.transformed(o => f(Transform(o, func)))
  }

  override def extruded(f: Primitive[TwoDimensional] => Primitive[ThreeDimensional]): Primitive[ThreeDimensional] = {
    obj.extruded(o => f(Transform(o, func)))
  }

  lazy val minBound: Vertex = reduceVertices(rendered.polygons.flatMap(_.vertices), _ min _)
  lazy val maxBound: Vertex = reduceVertices(rendered.polygons.flatMap(_.vertices), _ max _)
}

object Transform {

  def rotateFunc(radius: Double, angle: Double)(v: Vertex): Vertex = {
    val theta = angle / (2 * math.Pi * radius)
    val t = v.x
    val x = (radius + v.z) * math.sin(t * theta)
    val y = v.y
    val z = v.z + radius * math.cos(t * theta)
    Vertex(x, y, z)
  }

  def rotate[D <: Dim](obj: Primitive[D], radius: Double, angle: Double): Transform[D] = {
    Transform[D](obj, rotateFunc(radius, angle))
  }
}
