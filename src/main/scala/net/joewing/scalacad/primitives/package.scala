package net.joewing.scalacad

package object primitives {

  type Operator[D <: Dim] = (Primitive[D], Primitive[D]) => Primitive[D]

  /** Create a 2d triangle.
    * @param base The base of the triangle.
    * @param height The height of the triangle.
    */
  def triangle(base: Double, height: Double): Primitive2d = Triangle(base, height)

  /** Create a 2d rectangle.
    * @param width The width of the rectangle.
    * @param height The height of the rectangle.
    */
  def rectangle(width: Double, height: Double): Primitive2d = Rectangle(width, height)

  /** Create a 2d square.
    * @param width The size of the square.
    */
  def square(width: Double): Primitive2d = rectangle(width, width)

  /** Create a 2d polygon.
    * @param points The (x, y) points of the polygon.
    */
  def polygon(points: (Double, Double)*): Primitive2d = Polygon(points)

  /** Create a 2d circle (or n-sided object).
    * @param r The radius of the circle.
    * @param sides The number of sides (0 to pick a value based on r).
    */
  def circle(r: Double, sides: Int = 0): Primitive2d = Circle(r, sides)

  /** Create a 3d cube.
    * @param width The width of the cube.
    * @param height The height of the cube.
    * @param depth The depth of the cube.
    */
  def cube(width: Double, height: Double, depth: Double): Primitive3d = Cube(width, height, depth)

  /** Create a 3d cylinder.
    * @param length The length of the cylinder.
    * @param top The radius of the top of the cylinder.
    * @param bottom The radius of the bottom of the cylinter (same as top if negative).
    * @param sides The number of sides (0 to pick a value based on the radius).
    */
  def cylinder(length: Double, top: Double, bottom: Double = -1, sides: Int = 0): Primitive3d =
    Cylinder(length, top, bottom, sides)

  /** Create a 3d sphere.
    * @param r The radius of the sphere.
    * @param slices The number of horizontal slices (0 to pick a value based on r).
    * @param stacks The number of vertical stacks (0 to pick a value based on r).
    */
  def sphere(r: Double, slices: Int = 0, stacks: Int = 0): Primitive3d = Sphere(r, slices, stacks)

  /** Get the union of one or more objects.
    * @param objs The objects to union.
    */
  def union[D <: Dim](objs: Primitive[D]*): Primitive[D] = objs.reduce(Union.union[D])

  /** Get the union of one or more objects that do not touch.
    * This is a faster version of "union" for objects that are known to not touch.
    * Note that if the objects do touch, this may return an invalid object, thus the
    * "union" function should be preferred unless rendering is slow.
    * @param objs The objects to union.
    */
  def disjointUnion[D <: Dim](objs: Primitive[D]*): Primitive[D] = objs.reduce(Union.disjoint[D])

  /** Return the intersection of one or more objects.
    * @param objs The objects to intersect.
    */
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
      slices: Int = 1,
      scalex: Double = 1.0,
      scaley: Double = 1.0
    ): Primitive[ThreeDimensional] = LinearExtrude(left, length, slices, rotation, scalex, scaley)
  }

}
