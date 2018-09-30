package net.joewing.scalacad

import org.scalatest.{FunSpec, Matchers}

class PredicatesSpec extends FunSpec with Matchers {
  describe("two sum") {
    it("handles exact addition") {
      Predicates.sum(1.0, 2.0) shouldBe Seq(0.0, 3.0)
    }

    it("handles inexact addition") {
      val Vector(b, a) = Predicates.sum(1.0, 0.01)
      a shouldBe 1.01 +- 1e-12
      b should be < 0.0
      b should be > -1e-12
    }
  }

  describe("expansion sum") {
    it("sums two exact sequences") {
      Predicates.sum(Seq(1.0, 5.0), Seq(2.0, 5.0)) shouldBe Seq(13.0)
    }

    it("sums two inexact sequences") {
      val Seq(a, b) = Predicates.sum(Seq(0.1), Seq(1.0))
      a shouldBe 0.0 +- 1e-12
      b shouldBe 1.1 +- 1e-12
    }
  }

  describe("two product") {
    it("multiplies two values") {
      Predicates.product(2.0, 3.0) shouldBe Seq(0.0, 6.0)
    }
  }

  describe("expansion product") {
    it("multiplies two values") {
      Predicates.product(Seq(2.0), Seq(3.0)) shouldBe Seq(6.0)
    }
  }

  describe("scale") {
    it("scales a sequence") {
      Predicates.scale(Seq(0.5, 5.0), 2.0) shouldBe Seq(11.0)
    }
  }

  describe("compare") {
    it("returns >0 if a > b (simple case)") {
      Predicates.compare(Seq(0.0, 2.0), Seq(0.1, 1.0)) should be > 0.0
    }

    it("returns >0 if a > b (complicated case)") {
      Predicates.compare(Seq(0.2, 1.0), Seq(0.1, 1.0)) should be > 0.0
    }

    it("returns <0 if a < b") {
      Predicates.compare(Seq(0.0, 2.0), Seq(0.1, 3.0)) should be < 0.0
    }

    it("returns 0 if a == b") {
      Predicates.compare(Seq(0.1, 3.0), Seq(0.1, 3.0)) shouldBe 0.0
    }
  }
}
