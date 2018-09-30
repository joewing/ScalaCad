package net.joewing.scalacad

object Predicates {

  // Predicates from:
  // Jonathan Richard Shewchuk, "Adaptive Precision Floating-Point Arithmetic".

  val epsilon: Double = math.ulp(1.0) / 2.0
  val splitter: Double = math.pow(2, 27) + 1.0

  def split(a: Double): (Double, Double) = {
    val c = splitter * a
    val abig = c - a
    val ahi = c - abig
    val alo = a - ahi
    (alo, ahi)
  }

  def product(a: Double, b: Double): Vector[Double] = {
    val (alo, ahi) = split(a)
    val (blo, bhi) = split(b)

    val x = a * b
    val err1 = x - (ahi * bhi)
    val err2 = err1 - (alo * bhi)
    val err3 = err2 - (ahi * blo)
    val y = alo * blo - err3
    Vector(y, x)
  }

  def product(es: Seq[Double], fs: Seq[Double]): Seq[Double] = {
    compress(es.foldLeft(fs) { (hs, f) => scale(hs, f) })
  }

  /** Return the non-overlapping sum of a and b. */
  def sum(a: Double, b: Double): Vector[Double] = {
    val x = a + b
    val bv = x - a
    val av = x - bv
    val br = b - bv
    val ar = a - av
    Vector(ar + br, x)
  }

  def sum(es: Seq[Double], fs: Seq[Double]): Seq[Double] = {
    val gs = merge(es, fs)
    val glen = gs.length
    val hs = Array.fill[Double](glen)(0.0)
    val Vector(h1, initialQ) = fastTwoSum(gs(1), gs.head)
    hs(0) = h1
    var q = initialQ
    gs.indices.drop(2).foreach { i =>
      val Vector(nextH, newQ) = sum(q, gs(i))
      hs(i - 1) = nextH
      q = newQ
    }
    hs(glen - 1) = q
    compress(hs)
  }

  /** Fast version of twoSum when a is known to be larger than b. */
  def fastTwoSum(a: Double, b: Double): Vector[Double] = {
    val x = a + b
    val bv = x - a
    val y = b - bv
    Vector(y, x)
  }

  def diff(a: Double, b: Double): Vector[Double] = sum(a, -b)

  def growExpansion(es: Seq[Double], b: Double): Seq[Double] = {
    var q = b
    val result = es.map { e =>
      val Vector(hi, qi) = sum(q, e)
      q = qi
      hi
    }
    result :+ q
  }

  def scale(es: Seq[Double], b: Double): Seq[Double] = {
    val h = Array.fill[Double](2 * es.size)(0.0)
    val Vector(h1, initialQ) = product(es.head, b)
    h(0) = h1
    var q = initialQ
    es.indices.tail.foreach { i =>
      val Vector(littleT, bigT) = product(es(i), b)
      val Vector(ha, qPrime) = sum(q, littleT)
      val Vector(hb, nextQ) = fastTwoSum(bigT, qPrime)
      h(2 * i - 1) = ha
      h(2 * i) = hb
      q = nextQ
    }
    h(2 * es.length - 1) = q
    compress(h)
  }

  def merge(es: Seq[Double], fs: Seq[Double]): Array[Double] = {
    val elen = es.length
    val flen = fs.length
    val h = Array.fill[Double](elen + flen)(0.0)
    var ei = 0
    var fi = 0
    var hi = 0
    while (ei < elen && fi < flen) {
      if (math.abs(es(ei)) < math.abs(fs(fi))) {
        h(hi) = es(ei)
        ei += 1
      } else {
        h(hi) = fs(fi)
        fi += 1
      }
      hi += 1
    }
    while (ei < elen) {
      h(hi) = es(ei)
      ei += 1
      hi += 1
    }
    while (fi < flen) {
      h(hi) = fs(fi)
      fi += 1
      hi += 1
    }
    h
  }

  def compress(es: Seq[Double]): Seq[Double] = {
    val temp = es.filterNot(_ == 0.0)
    if (temp.nonEmpty) temp else Vector(0.0)
  }

  def expansionDifference(es: Seq[Double], fs: Seq[Double]): Seq[Double] = sum(es, fs.map(-_))

  def compare(es: Seq[Double], fs: Seq[Double]): Double = expansionDifference(es, fs).last

  def compare(es: Seq[Double], f: Double): Double = compare(es, Vector(f))

  def abs(x: Double): Double = math.abs(x)

  def threeProduct(a: Double, b: Double, c: Double): Seq[Double] = scale(product(a, b), c)

  def det3x3(
    a: Double, b: Double, c: Double,
    d: Double, e: Double, f: Double,
    g: Double, h: Double, i: Double
  ): Seq[Double] = {
    val aei = threeProduct(a, e, i)
    val bfg = threeProduct(b, f, g)
    val cdh = threeProduct(c, d, h)
    val ceg = threeProduct(c, e, g)
    val bdi = threeProduct(b, d, i)
    val afh = threeProduct(a, f, h)
    val s1 = sum(sum(aei, bfg), cdh)
    val s2 = sum(sum(ceg, bdi), afh)
    expansionDifference(s1, s2)
  }

  def det4x4(
    x11: Double, x12: Double, x13: Double, x14: Double,
    x21: Double, x22: Double, x23: Double, x24: Double,
    x31: Double, x32: Double, x33: Double, x34: Double,
    x41: Double, x42: Double, x43: Double, x44: Double
  ): Seq[Double] = {
    val m1 = scale(det3x3(x22, x23, x24, x32, x33, x34, x42, x43, x44), x11)
    val m2 = scale(det3x3(x21, x23, x24, x31, x33, x34, x41, x43, x44), x12)
    val m3 = scale(det3x3(x21, x22, x24, x31, x32, x34, x41, x42, x44), x13)
    val m4 = scale(det3x3(x21, x22, x23, x31, x32, x33, x41, x42, x43), x14)
    expansionDifference(sum(m1, m3), sum(m2, m4))
  }
}
