package net.joewing.scalacad

package object primitives {

  type Operator[D <: Dim] = (Primitive[D], Primitive[D]) => Primitive[D]

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

    def |(right: Primitive[D]): Primitive[D] = Union(left, right)
    def &(right: Primitive[D]): Primitive[D] = Intersection(left, right)
    def -(right: Primitive[D]): Primitive[D] = Difference(left, right)

    def translate(x: Double = 0, y: Double = 0, z: Double = 0): Primitive[D] = Translate(left, x, y, z)
    def translate(v: Vertex): Primitive[D] = translate(v.x1, v.x2, v.x3)
    def rotate(x: Double = 0, y: Double = 0, z: Double = 0): Primitive[D] = Rotate(left, x, y, z)
    def scale(x: Double = 1, y: Double = 1, z: Double = 1): Primitive[D] = Scale(left, x, z, y)

    def centered: Primitive[D] = translate((left.maxBound + left.minBound) / -2)

    def above(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      val newZ = other.maxBound.x3 - overlap
      val deltaZ = newZ - left.minBound.x3
      op(other, translate(z = deltaZ))
    }
    def below(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      val newZ = other.minBound.x3 - left.extent.x3 + overlap
      val deltaZ = newZ - left.minBound.x3
      op(other, translate(z = deltaZ))
    }
    def beside(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      op(other, translate(x = other.extent.x1 - overlap))
    }
    def behind(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      op(other, translate(y = other.extent.x2 - overlap))
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
