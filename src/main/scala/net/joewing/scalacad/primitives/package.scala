package net.joewing.scalacad

package object primitives {

  type Operator[D <: Dim] = (Primitive[D], Primitive[D]) => Primitive[D]

  def triangle(base: Double, height: Double): Triangle = Triangle(base, height)

  def rectangle(width: Double, height: Double): Rectangle = Rectangle(width, height)

  def square(width: Double): Rectangle = rectangle(width, width)

  def polygon(points: (Double, Double)*): Polygon = Polygon(points)

  def circle(r: Double, sides: Int = 8): Circle = Circle(r, sides)

  def cube(width: Double, height: Double, depth: Double): Cube = Cube(width, height, depth)

  def cylinder(length: Double, bottom: Double, top: Double, sides: Int): Cylinder =
    Cylinder(length, bottom, top, sides)

  def sphere(r: Double, slices: Int = 8, stacks: Int = 8): Sphere = Sphere(r, slices, stacks)

  def union[D <: Dim](objs: Seq[Primitive[D]]): Primitive[D] = objs.reduce(Union.apply[D])
  def intersect[D <: Dim](objs: Seq[Primitive[D]]): Primitive[D] = objs.reduce(Intersection.apply[D])

  trait RichPrimitive[D <: Dim] {
    val left: Primitive[D]

    def |(right: Primitive[D]): Primitive[D] = Union(left, right)
    def &(right: Primitive[D]): Primitive[D] = Intersection(left, right)
    def -(right: Primitive[D]): Primitive[D] = Difference(left, right)

    def translate(x: Double = 0, y: Double = 0, z: Double = 0): Primitive[D] = Translate(left, x, y, z)
    def translate(v: Vertex): Primitive[D] = translate(v.x, v.y, v.z)

    def centered: Primitive[D] = translate((left.maxBound + left.minBound) / -2)

    def above(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      val newZ = other.maxBound.z - overlap
      val deltaZ = newZ - left.minBound.z
      op(other, translate(z = deltaZ))
    }
    def below(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      val newZ = other.minBound.z - left.extent.z + overlap
      val deltaZ = newZ - left.minBound.z
      op(other, translate(z = deltaZ))
    }
    def beside(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      val newX = other.maxBound.x - overlap
      val deltaX = newX - left.minBound.x
      op(other, translate(x = deltaX))
    }
    def behind(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.apply): Primitive[D] = {
      val newY = other.maxBound.y - overlap
      val deltaY = newY - left.minBound.y
      op(other, translate(y = deltaY))
    }
  }

  implicit class RichSolid(val left: Primitive[ThreeDimensional]) extends RichPrimitive[ThreeDimensional] {
    def rotate(x: Double = 0, y: Double = 0, z: Double = 0): Primitive[ThreeDimensional] = Rotate(left, x, y, z)
    def scale(x: Double = 1, y: Double = 1, z: Double = 1): Primitive[ThreeDimensional] = Scale(left, x, y, z)
  }

  implicit class RichSurface(val left: Primitive[TwoDimensional]) extends RichPrimitive[TwoDimensional] {
    def rotate(z: Double = 0): Primitive[TwoDimensional] = Rotate(left, 0, 0, z)
    def scale(x: Double = 1, y: Double = 1): Primitive[TwoDimensional] = Scale(left, x, y, 1)
    def extrude(
      length: Double,
      rotation: Double = 0.0,
      slices: Int = 1
    ): LinearExtrude = LinearExtrude(left, length, rotation, slices)
  }

}
