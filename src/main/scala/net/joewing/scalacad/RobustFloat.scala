package net.joewing.scalacad

import scala.annotation.strictfp

@strictfp
object RobustFloat {

  // Predicates base off of:
  // Jonathan Richard Shewchuk, "Adaptive Precision Floating-Point Arithmetic".

  val epsilon: Double = math.ulp(1.0) / 2.0
  val splitter: Double = math.pow(2, 27) + 1.0

  @inline
  def split(a: Double): (Double, Double) = {
    val c = splitter * a
    val abig = c - a
    val ahi = c - abig
    val alo = a - ahi
    (alo, ahi)
  }

  @inline
  def product(a: Double, b: Double): Array[Double] = {
    val (alo, ahi) = split(a)
    val (blo, bhi) = split(b)

    val x = a * b
    val err1 = x - (ahi * bhi)
    val err2 = err1 - (alo * bhi)
    val err3 = err2 - (ahi * blo)
    val y = alo * blo - err3
    Array(y, x)
  }

  def product(es: Array[Double], fs: Array[Double]): Array[Double] = {
    val elen = es.length
    val flen = fs.length
    if (elen == 1) {
      scale(fs, es.head)
    } else if (flen == 1) {
      scale(es, fs.head)
    } else if (elen < flen) {
      var result = Array(0.0)
      var i = 0
      while (i < es.length) {
        result = sum(result, scale(fs, es(i)))
        i += 1
      }
      result
    } else {
      var result = Array(0.0)
      var i = 0
      while (i < fs.length) {
        result = sum(result, scale(es, fs(i)))
        i += 1
      }
      result
    }
  }

  /** Return the non-overlapping sum of a and b. */
  @inline
  def sum(a: Double, b: Double): Array[Double] = {
    val x = a + b
    val bv = x - a
    val av = x - bv
    val br = b - bv
    val ar = a - av
    Array(ar + br, x)
  }

  def sum(es: Array[Double], fs: Array[Double]): Array[Double] = {
    val gs = merge(es, fs)
    val glen = gs.length
    val hs = Array.fill[Double](glen)(0.0)
    val initialQ = gs(1) + gs.head
    hs(0) = gs.head - (initialQ - gs(1))
    var q = initialQ
    gs.indices.drop(2).foreach { i =>
      val Array(nextH, newQ) = sum(q, gs(i))
      hs(i - 1) = nextH
      q = newQ
    }
    hs(glen - 1) = q
    compress(hs)
  }

  @inline
  def diff(a: Double, b: Double): Array[Double] = sum(a, -b)

  def scale(es: Array[Double], b: Double): Array[Double] = {
    val h = Array.fill[Double](2 * es.length)(0.0)
    val Array(h1, initialQ) = product(es.head, b)
    h(0) = h1
    var q = initialQ
    var i = 1
    while (i < es.length) {
      val Array(littleT, bigT) = product(es(i), b)
      val Array(ha, qPrime) = sum(q, littleT)
      val nextQ = bigT + qPrime
      val hb = qPrime - (nextQ - bigT)
      h(2 * i - 1) = ha
      h(2 * i) = hb
      q = nextQ
      i += 1
    }
    h(2 * es.length - 1) = q
    compress(h)
  }

  def merge(es: Array[Double], fs: Array[Double]): Array[Double] = {
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

  def compress(es: Array[Double]): Array[Double] = {
    val elen = es.length
    val gs = Array.fill[Double](elen)(0.0)
    var q = es.last
    var b = elen - 1
    var i = elen - 2
    while (i >= 0) {
      val newQ = q + es(i)
      val residual = es(i) - (newQ - q)
      q = newQ
      if (residual != 0.0) {
        gs(b) = q
        b -= 1
        q = residual
      }
      i -= 1
    }
    val hs = Array.fill[Double](elen)(0.0)
    var t = 0
    i = b + 1
    while (i < elen) {
      val newQ = gs(i) + q
      val residual = q - (newQ - gs(i))
      q = newQ
      if (residual != 0.0) {
        hs(t) = residual
        t += 1
      }
      i += 1
    }
    hs(t) = q
    hs.take(t + 1)
  }

  @inline
  def diff(es: Array[Double], fs: Array[Double]): Array[Double] = sum(es, fs.map(-_))

  @inline
  def compare(es: Array[Double], fs: Array[Double]): Double = diff(es, fs).last

  @inline
  def compare(es: Array[Double], f: Double): Double = compare(es, Array(f))

  @inline
  def abs(es: Array[Double]): Array[Double] = if (es.last < 0) es.map(-_) else es

  @inline
  def abs(x: Double): Double = math.abs(x)

  @inline
  def threeProduct(a: Double, b: Double, c: Double): Array[Double] = scale(product(a, b), c)

  def det3x3(
    a: Double, b: Double, c: Double,
    d: Double, e: Double, f: Double,
    g: Double, h: Double, i: Double
  ): Array[Double] = {
    val aei = threeProduct(a, e, i)
    val bfg = threeProduct(b, f, g)
    val cdh = threeProduct(c, d, h)
    val ceg = threeProduct(c, e, g)
    val bdi = threeProduct(b, d, i)
    val afh = threeProduct(a, f, h)
    val s1 = sum(sum(aei, bfg), cdh)
    val s2 = sum(sum(ceg, bdi), afh)
    diff(s1, s2)
  }

  def det4x4(
    x11: Double, x12: Double, x13: Double, x14: Double,
    x21: Double, x22: Double, x23: Double, x24: Double,
    x31: Double, x32: Double, x33: Double, x34: Double,
    x41: Double, x42: Double, x43: Double, x44: Double
  ): Array[Double] = {
    val m1 = scale(det3x3(x22, x23, x24, x32, x33, x34, x42, x43, x44), x11)
    val m2 = scale(det3x3(x21, x23, x24, x31, x33, x34, x41, x43, x44), x12)
    val m3 = scale(det3x3(x21, x22, x24, x31, x32, x34, x41, x42, x44), x13)
    val m4 = scale(det3x3(x21, x22, x23, x31, x32, x33, x41, x42, x43), x14)
    diff(sum(m1, m3), sum(m2, m4))
  }
}
