package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

class RobustFloatSpec extends FunSpec with Matchers {
  describe("two sum") {
    it("handles exact addition") {
      RobustFloat.sum(1.0, 2.0) shouldBe Array(0.0, 3.0)
    }

    it("handles inexact addition") {
      val Array(b, a) = RobustFloat.sum(1.0, 0.01)
      a shouldBe 1.01 +- 1e-12
      b should be < 0.0
      b should be > -1e-12
    }
  }

  describe("expansion sum") {
    it("sums two exact sequences") {
      RobustFloat.sum(Array(1.0, 5.0), Array(2.0, 5.0)) shouldBe Array(13.0)
    }

    it("sums two inexact sequences") {
      val Array(a, b) = RobustFloat.sum(Array(0.1), Array(1.0))
      b shouldBe 1.1
    }
  }

  describe("two product") {
    it("multiplies two values") {
      RobustFloat.product(2.0, 3.0) shouldBe Array(0.0, 6.0)
    }
  }

  describe("expansion product") {
    it("multiplies two values") {
      RobustFloat.product(Array(2.0), Array(3.0)) shouldBe Array(6.0)
    }

    it("multiplies two values (multiple)") {
      RobustFloat.product(Array(0.1, 2.0), Array(0.2, 3.0)).last shouldBe 6.72 +- 1e-12
    }
  }

  describe("scale") {
    it("scales a sequence") {
      RobustFloat.scale(Array(0.5, 5.0), 2.0) shouldBe Array(11.0)
    }
  }

  describe("compare") {
    it("returns >0 if a > b (simple case)") {
      RobustFloat.compare(Array(0.0, 2.0), Array(0.1, 1.0)) should be > 0
    }

    it("returns >0 if a > b (complicated case)") {
      RobustFloat.compare(Array(0.2, 1.0), Array(0.1, 1.0)) should be > 0
    }

    it("returns <0 if a < b") {
      RobustFloat.compare(Array(0.0, 2.0), Array(0.1, 3.0)) should be < 0
    }

    it("returns 0 if a == b") {
      RobustFloat.compare(Array(0.1, 3.0), Array(0.1, 3.0)) shouldBe 0
    }
  }

  describe("divide") {
    it("returns the right result") {
      RobustFloat(12) / RobustFloat(3) shouldBe RobustFloat(4)
    }
  }

  describe("crossProduct") {
    it("returns the right result") {
      val a = Vertex(1, 2, 3)
      val b = Vertex(3, 4, 5)
      val result = RobustFloat.crossProduct(a, b)
      val expected = a.cross(b)
      result._1.toDouble shouldBe expected.x
      result._2.toDouble shouldBe expected.y
      result._3.toDouble shouldBe expected.z
    }
  }

  describe("dotProduct") {
    it("returns the right result") {
      val a = Vertex(1, 2, 3)
      val result = RobustFloat.dotProduct(a, (RobustFloat(3), RobustFloat(4), RobustFloat(5)))
      result.toDouble shouldBe a.dot(Vertex(3, 4, 5))
    }
  }
}
