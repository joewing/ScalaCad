package net.joewing.scalacad

final case class Plane(normal: Vertex, w: Double) {

  def flip: Plane = Plane(normal.negated, -w)

  // This determines how "thick" the plane is.
  private val epsilon: Double = Vertex.epsilon
  private val fastMath: Boolean = true
  private val slowMath: Boolean = false

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

  def split(polygons: Seq[PlanePolygon], result: Plane.PlaneSplitResult): Unit = {
    polygons.foreach { polygon =>
      splitPlanePolygon(polygon, result)
    }
  }

  def split(polygons: Seq[PlanePolygon]): Plane.PlaneSplitResult = {
    val result = Plane.PlaneSplitResult()
    split(polygons, result)
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

  def classify(p: Plane, q: Plane, r: Plane): Plane.Classification = {
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

  // These are mutable to improve performance during construction.
  final case class PlaneSplitResult(
    front: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer(),
    back: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer(),
    coFront: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer(),
    coBack: scala.collection.mutable.ArrayBuffer[PlanePolygon] = scala.collection.mutable.ArrayBuffer()
  )

  sealed trait Classification
  case object Coplanar extends Classification
  case object Front extends Classification
  case object Back extends Classification

  def apply(polygon: Polygon3d): Plane = Plane(polygon.normal, -polygon.normal.dot(polygon.vertices.head))

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
