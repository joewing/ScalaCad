package net.joewing.csg

package object primitives {

  def cube(size: Double = 1): Renderable = Cylinder(size, size, size, 4)

  def cylinder(length: Double, bottom: Double, top: Double, sides: Int): Renderable =
    Cylinder(length, bottom, top, sides)

  implicit class RichRenderable(left: Renderable) {
    def |(right: Renderable): Renderable = Union(left, right)
    def &(right: Renderable): Renderable = Intersection(left, right)
    def -(right: Renderable): Renderable = Difference(left, right)
    def unary_-(): Renderable = Invert(left)
  }
}
