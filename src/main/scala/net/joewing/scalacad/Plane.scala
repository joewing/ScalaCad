package net.joewing.scalacad

import scala.collection.mutable

final case class Plane(normal: Vertex, w: Double) {

  def flip: Plane = Plane(normal.negated, -w)

  // This determines how "thick" the plane is.
  private val epsilon: Double = Vertex.epsilon
  private val fastMath: Boolean = true
  private val slowMath: Boolean = false

  def classify(v: Vertex): Int = {
    val t = normal.dot(v) - w
    if (t < -epsilon) Plane.Back
    else if (t > epsilon) Plane.Front
    else Plane.Coplanar
  }

  def splitPolygon(polygon: Polygon3d, result: Plane.SplitResult): Unit = {
    val len = polygon.vertices.length
    val types = Array.ofDim[Int](len)
    var hasFront = false
    var hasBack = false
    var i = 0
    while (i < len) {
      types(i) = classify(polygon.vertices(i))
      hasFront = hasFront || types(i) == Plane.Front
      hasBack = hasBack || types(i) == Plane.Back
      i += 1
    }
    if (!hasFront && !hasBack) {
      if (normal.dot(polygon.normal) > 0) {
        result.coFront += polygon
      } else {
        result.coBack += polygon
      }
    } else if (hasFront && !hasBack) {
      result.front += polygon
    } else if (hasBack && !hasFront) {
      result.back += polygon
    } else {
      val fs = new mutable.ArrayBuilder.ofRef[Vertex]
      val bs = new mutable.ArrayBuilder.ofRef[Vertex]
      i = 0
      while (i < len) {
        val j = (i + 1) % len
        val ti = types(i)
        val tj = types(j)
        val vi = polygon.vertices(i)
        val vj = polygon.vertices(j)
        if (ti != Plane.Back) fs += vi
        if (ti != Plane.Front) bs += vi
        if ((ti | tj) == Plane.Spanning) {
          val diff = vj - vi
          val t = (w - normal.dot(vi)) / normal.dot(diff)
          val v = vi + diff * t
          fs += v
          bs += v
        }
        i += 1
      }
      result.front += Polygon3d(fs.result)
      result.back += Polygon3d(bs.result)
    }
  }

  private def splitPlanePolygon(pp: PlanePolygon, result: Plane.PlaneSplitResult): Unit = {
    if (coincident(pp.support)) {
      if (sameOrientation(pp.support)) {
        // Co-planar front
        result.coFront += pp
      } else {
        // Co-planar back
        result.coBack += pp
      }
    } else {
      val fs = scala.collection.mutable.ArrayBuffer[Plane]()
      val bs = scala.collection.mutable.ArrayBuffer[Plane]()
      val boundingPlanes = pp.bounding
      val count = boundingPlanes.size
      var i = 0
      while (i < count) {
        val a = boundingPlanes((i + count - 2) % count)
        val b = boundingPlanes((i + count - 1) % count)
        val c = boundingPlanes(i)
        val d = boundingPlanes((i + 1) % count)
        val c1 = classify(pp.support, a, b)
        val c2 = classify(pp.support, b, c)
        val c3 = classify(pp.support, c, d)
        (c1, c2, c3) match {
          case (Plane.Front, Plane.Coplanar, Plane.Front)    => fs += c
          case (Plane.Back, Plane.Coplanar, Plane.Back)      => bs += c
          case (Plane.Coplanar, Plane.Coplanar, Plane.Front) => fs ++= Seq(this, c)
          case (Plane.Coplanar, Plane.Coplanar, Plane.Back)  => bs ++= Seq(this, c)
          case (Plane.Back, Plane.Coplanar, Plane.Front)     => fs ++= Seq(this, c)
          case (Plane.Front, Plane.Coplanar, Plane.Back)     => bs ++= Seq(this, c)
          case (_, Plane.Front, Plane.Front)                 => fs += c
          case (_, Plane.Back, Plane.Back)                   => bs += c
          case (_, Plane.Front, Plane.Coplanar)              => fs += c
          case (_, Plane.Back, Plane.Coplanar)               => bs += c
          case (_, Plane.Front, Plane.Back)                  => fs += c; bs ++= Seq(this, c)
          case (_, Plane.Back, Plane.Front)                  => bs += c; fs ++= Seq(this, c)
          case _                                             => ()
        }
        i += 1
      }
      if (fs.nonEmpty) result.front += PlanePolygon(pp.support +: fs)
      if (bs.nonEmpty) result.back += PlanePolygon(pp.support +: bs)
    }
  }

  def split(polygons: IndexedSeq[Polygon3d]): Plane.SplitResult = {
    val result = new Plane.SplitResult
    var i = 0
    val len = polygons.length
    while (i < len) {
      splitPolygon(polygons(i), result)
      i += 1
    }
    result
  }

  def splitPlanes(polygons: Seq[PlanePolygon], result: Plane.PlaneSplitResult): Unit = {
    polygons.foreach { polygon =>
      splitPlanePolygon(polygon, result)
    }
  }

  def splitPlanes(polygons: Seq[PlanePolygon]): Plane.PlaneSplitResult = {
    val result = Plane.PlaneSplitResult()
    splitPlanes(polygons, result)
    result
  }

  def coincident(other: Plane): Boolean = {
    val (pa, pb, pc, pd) = (normal.x, normal.y, normal.z, w)
    val (qa, qb, qc, qd) = (other.normal.x, other.normal.y, other.normal.z, other.w)

    def helper(a: Double, b: Double, c: Double, d: Double): Boolean = {
      val fast = math.abs(a * b - c * d)
      if (fastMath || (fast > 2 * epsilon && !slowMath)) {
        fast < epsilon
      } else {
        (RobustFloat(a) * RobustFloat(b) - RobustFloat(c) * RobustFloat(d)).abs < epsilon
      }
    }

    helper(pa, qb, pb, qa) &&
      helper(pa, qc, pc, qa) &&
      helper(pa, qd, pd, qa) &&
      helper(pb, qc, pc, qb) &&
      helper(pb, qd, pd, qb) &&
      helper(pc, qd, pd, qc)
  }

  def sameOrientation(other: Plane): Boolean = {
    val (pa, pb, pc, pd) = (normal.x, normal.y, normal.z, w)
    val (qa, qb, qc, qd) = (other.normal.x, other.normal.y, other.normal.z, other.w)

    def helper(a: Double, b: Double): Boolean = {
      val fast = a * b
      if (fastMath || (math.abs(fast) > 2 * epsilon && !slowMath)) {
        fast >= 0.0
      } else {
        RobustFloat(a) * RobustFloat(b) >= 0
      }
    }

    helper(pa, qa) && helper(pb, qb) && helper(pc, qc) && helper(pd, qd)
  }

  def classify(p: Plane, q: Plane, r: Plane): Int = {
    val left = Plane.det3x3(
      p.normal.x, p.normal.y, p.normal.z,
      q.normal.x, q.normal.y, q.normal.z,
      r.normal.x, r.normal.y, r.normal.z
    )
    val right = Plane.det4x4(
      p.normal.x, p.normal.y, p.normal.z, p.w,
      q.normal.x, q.normal.y, q.normal.z, q.w,
      r.normal.x, r.normal.y, r.normal.z, r.w,
      normal.x, normal.y, normal.z, w
    )
    val fast = left * right
    if (fastMath || (math.abs(fast) > 4 * epsilon && !slowMath)) {
      if (fast > epsilon) Plane.Front
      else if (fast < -epsilon) Plane.Back
      else Plane.Coplanar
    } else {
      val left = RobustFloat.det3x3(
        p.normal.x, p.normal.y, p.normal.z,
        q.normal.x, q.normal.y, q.normal.z,
        r.normal.x, r.normal.y, r.normal.z
      )
      val right = RobustFloat.det4x4(
        p.normal.x, p.normal.y, p.normal.z, p.w,
        q.normal.x, q.normal.y, q.normal.z, q.w,
        r.normal.x, r.normal.y, r.normal.z, r.w,
        normal.x, normal.y, normal.z, w
      )
      val comp = left * right
      if (comp > epsilon) Plane.Front
      else if (comp < -epsilon) Plane.Back
      else Plane.Coplanar
    }
  }

}

object Plane {

  final class SplitResult {
    val front: mutable.ArrayBuilder[Polygon3d] = new mutable.ArrayBuilder.ofRef[Polygon3d]
    val back: mutable.ArrayBuilder[Polygon3d] = new mutable.ArrayBuilder.ofRef[Polygon3d]
    val coFront: mutable.ArrayBuilder[Polygon3d] = new mutable.ArrayBuilder.ofRef[Polygon3d]
    val coBack: mutable.ArrayBuilder[Polygon3d] = new mutable.ArrayBuilder.ofRef[Polygon3d]
  }

  // These are mutable to improve performance during construction.
  final case class PlaneSplitResult(
    front: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer(),
    back: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer(),
    coFront: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer(),
    coBack: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer()
  )

  val Coplanar: Int = 0
  val Front: Int = 1
  val Back: Int = 2
  val Spanning: Int = 3

  def apply(polygon: Polygon3d): Plane = Plane(polygon.normal, polygon.normal.dot(polygon.vertices.head))

  def apply(a: Vertex, b: Vertex, c: Vertex): Plane = {
    val n = (b - a).cross(c - a).unit
    val w = -a.dot(n)
    Plane(n, w)
  }

  def apply(vertices: Seq[Vertex]): Plane = {
    val Seq(a, b, c) = vertices.take(3)
    apply(a, b, c)
  }

  def det3x3(
    a: Double, b: Double, c: Double,
    d: Double, e: Double, f: Double,
    g: Double, h: Double, i: Double
  ): Double = {
    a * e * i + b * f * g + c * d * h - c * e * g - b * d * i - a * f * h
  }

  def det4x4(
    x11: Double, x12: Double, x13: Double, x14: Double,
    x21: Double, x22: Double, x23: Double, x24: Double,
    x31: Double, x32: Double, x33: Double, x34: Double,
    x41: Double, x42: Double, x43: Double, x44: Double
  ): Double = {
    val m1 = x11 * det3x3(x22, x23, x24, x32, x33, x34, x42, x43, x44)
    val m2 = x12 * det3x3(x21, x23, x24, x31, x33, x34, x41, x43, x44)
    val m3 = x13 * det3x3(x21, x22, x24, x31, x32, x34, x41, x42, x44)
    val m4 = x14 * det3x3(x21, x22, x23, x31, x32, x33, x41, x42, x43)
    m1 - m2 + m3 - m4
  }
}
