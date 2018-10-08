package net.joewing.scalacad.primitives

import net.joewing.scalacad.{RenderedObject, Vertex}

import scala.concurrent.{ExecutionContext, Future}

final case class Scale[D <: Dim](obj: Primitive[D], x: Double, y: Double, z: Double) extends Primitive[D] {
  val dim: D = obj.dim

  protected def render(implicit ec: ExecutionContext): Future[RenderedObject] = {
    obj.renderedFuture.map(_.map(_.scaled(x, y, z)))
  }

  override def transformed(f: Primitive[D] => Primitive[D]): Primitive[D] =
    obj.transformed(o => f(Scale(o, x, y, z)))

  lazy val minBound: Vertex = obj.minBound.scaled(x, y, z)
  lazy val maxBound: Vertex = obj.maxBound.scaled(x, y, z)
}

