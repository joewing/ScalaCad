package net.joewing.scalacad

package object primitives {

  type Operator[D <: Dim] = (Primitive[D], Primitive[D]) => Primitive[D]

  def triangle(base: Double, height: Double): Primitive2d = Triangle(base, height)

  def rectangle(width: Double, height: Double): Primitive2d = Rectangle(width, height)

  def square(width: Double): Primitive2d = rectangle(width, width)

  def polygon(points: (Double, Double)*): Primitive2d = Polygon(points)

  def circle(r: Double, sides: Int = 0): Primitive2d = Circle(r, sides)

  def cube(width: Double, height: Double, depth: Double): Primitive3d = Cube(width, height, depth)

  def cylinder(length: Double, top: Double, bottom: Double = -1, sides: Int = 0): Primitive3d =
    Cylinder(length, top, bottom, sides)

  def sphere(r: Double, slices: Int = 0, stacks: Int = 0): Primitive3d = Sphere(r, slices, stacks)

  def union[D <: Dim](objs: Primitive[D]*): Primitive[D] = objs.reduce(Union.union[D])
  def disjointUnion[D <: Dim](objs: Primitive[D]*): Primitive[D] = objs.reduce(Union.disjoint[D])
  def intersect[D <: Dim](objs: Primitive[D]*): Primitive[D] = objs.reduce(Intersection.apply[D])

  trait RichPrimitive[D <: Dim] {
    val left: Primitive[D]

    def |(right: Primitive[D]): Primitive[D] = Union.union(left, right)
    def &(right: Primitive[D]): Primitive[D] = Intersection(left, right)
    def -(right: Primitive[D]): Primitive[D] = Difference(left, right)

    def translate(x: Double = 0, y: Double = 0, z: Double = 0): Primitive[D] = Translate(left, x, y, z)
    def translate(v: Vertex): Primitive[D] = translate(v.x, v.y, v.z)

    def centered: Primitive[D] = translate((left.maxBound + left.minBound) / -2)

    def above(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.union): Primitive[D] = {
      val newZ = other.maxBound.z - overlap
      val deltaZ = newZ - left.minBound.z
      op(other, translate(z = deltaZ))
    }
    def below(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.union): Primitive[D] = {
      val newZ = other.minBound.z - left.extent.z + overlap
      val deltaZ = newZ - left.minBound.z
      op(other, translate(z = deltaZ))
    }
    def beside(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.union): Primitive[D] = {
      val newX = other.maxBound.x - overlap
      val deltaX = newX - left.minBound.x
      op(other, translate(x = deltaX))
    }
    def behind(other: Primitive[D], overlap: Double = 0.0, op: Operator[D] = Union.union): Primitive[D] = {
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
    ): Primitive[ThreeDimensional] = LinearExtrude(left, length, rotation, slices)
  }

}
