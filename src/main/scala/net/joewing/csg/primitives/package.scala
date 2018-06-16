package net.joewing.csg

package object primitives {

  def cube(size: Double): Renderable = Cylinder(size, size, size, 4)

  implicit class RichRenderable(left: Renderable) {
    def |(right: Renderable): Renderable = Union(left, right)
    def &(right: Renderable): Renderable = Intersection(left, right)
    def -(right: Renderable): Renderable = Difference(left, right)
  }

}
