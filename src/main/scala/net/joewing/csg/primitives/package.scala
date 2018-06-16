package net.joewing.csg

package object primitives {

  def cube(width: Double, height: Double, depth: Double): Renderable = Cube(width, height, depth)

  def cylinder(length: Double, bottom: Double, top: Double, sides: Int): Renderable =
    Cylinder(length, bottom, top, sides)

  def sphere(r: Double, slices: Int = 8, stacks: Int = 8): Renderable = Sphere(r, slices, stacks)

  implicit class RichRenderable(left: Renderable) {
    def |(right: Renderable): Renderable = Union(left, right)
    def &(right: Renderable): Renderable = Intersection(left, right)
    def -(right: Renderable): Renderable = Difference(left, right)
    def unary_-(): Renderable = Invert(left)

    def translate(x: Double = 0, y: Double = 0, z: Double = 0): Renderable = Translate(left, x, y, z)
  }
}
