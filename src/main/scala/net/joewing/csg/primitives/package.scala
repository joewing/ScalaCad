package net.joewing.csg

package object primitives {

  def triangle(base: Double, height: Double): Triangle = Triangle(base, height)

  def rectangle(width: Double, height: Double): Rectangle = Rectangle(width, height)

  def cube(width: Double, height: Double, depth: Double): Cube = Cube(width, height, depth)

  def cylinder(length: Double, bottom: Double, top: Double, sides: Int): Cylinder =
    Cylinder(length, bottom, top, sides)

  def sphere(r: Double, slices: Int = 8, stacks: Int = 8): Sphere = Sphere(r, slices, stacks)

  implicit class RichRenderable[D <: Dim](left: Primitive[D]) {
    def |(right: Primitive[D]): Union[D] = Union(left, right)
    def &(right: Primitive[D]): Intersection[D] = Intersection(left, right)
    def -(right: Primitive[D]): Difference[D] = Difference(left, right)
    def unary_-(): Invert[D] = Invert(left)

    def translate(x: Double = 0, y: Double = 0, z: Double = 0): Translate[D] = Translate(left, x, y, z)
    def rotate(x: Double = 0, y: Double = 0, z: Double = 0): Rotate[D] = Rotate(left, x, y, z)
    def scale(x: Double = 1, y: Double = 1, z: Double = 1): Scale[D] = Scale(left, x, z, y)
  }

}
