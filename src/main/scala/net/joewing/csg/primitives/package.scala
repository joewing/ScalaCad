package net.joewing.csg

package object primitives {

  def triangle(base: Double, height: Double): Triangle = Triangle(base, height)

  def rectangle(width: Double, height: Double): Rectangle = Rectangle(width, height)

  def square(width: Double): Rectangle = rectangle(width, width)

  def circle(r: Double, sides: Int = 8): Circle = Circle(r, sides)

  def cube(width: Double, height: Double, depth: Double): Cube = Cube(width, height, depth)

  def cylinder(length: Double, bottom: Double, top: Double, sides: Int): Cylinder =
    Cylinder(length, bottom, top, sides)

  def sphere(r: Double, slices: Int = 8, stacks: Int = 8): Sphere = Sphere(r, slices, stacks)

  trait RichPrimitive[D <: Dim] {
    val left: Primitive[D]

    def |(right: Primitive[D]): Union[D] = Union(left, right)
    def &(right: Primitive[D]): Intersection[D] = Intersection(left, right)
    def -(right: Primitive[D]): Difference[D] = Difference(left, right)
    def unary_-(): Invert[D] = Invert(left)

    def translate(x: Double = 0, y: Double = 0, z: Double = 0): Translate[D] = Translate(left, x, y, z)
    def translate(v: Vertex): Translate[D] = translate(v.x1, v.x2, v.x3)
    def rotate(x: Double = 0, y: Double = 0, z: Double = 0): Rotate[D] = Rotate(left, x, y, z)
    def scale(x: Double = 1, y: Double = 1, z: Double = 1): Scale[D] = Scale(left, x, z, y)

    def centered: Translate[D] = translate((left.maxBound + left.minBound) / -2)

    def above(other: Primitive[D], overlap: Double = 0.0): Primitive[D] = {
      Union(translate(z = other.extent.x3 - overlap), other)
    }
    def below(other: Primitive[D], overlap: Double = 0.0): Primitive[D] = {
      Union(translate(z = -left.extent.x3 - overlap), other)
    }
    def beside(right: Primitive[D], overlap: Double = 0.0): Primitive[D] = {
      Union(translate(x = right.extent.x1 - overlap), right)
    }
    def behind(other: Primitive[D], overlap: Double = 0.0): Primitive[D] = {
      Union(translate(y = other.extent.x2 - overlap), other)
    }
  }

  implicit class RichSolid(val left: Primitive[ThreeDimensional]) extends RichPrimitive[ThreeDimensional] {
  }

  implicit class RichSurface(val left: Primitive[TwoDimensional]) extends RichPrimitive[TwoDimensional] {
    def extrude(
      length: Double,
      rotation: Double = 0.0,
      slices: Int = 1
    ): LinearExtrude = LinearExtrude(left, length, rotation, slices)
  }

}
