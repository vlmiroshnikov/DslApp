package dsl

import dsl.CollectionHelper._
import dsl.filter.FilterBuilder
import org.scalatest.{FlatSpec, Matchers}

class FilterBuilderTest extends FlatSpec with Matchers {

  it should " < filter" in {
    val s = Seq(Person("RU", 12), Person("EN", 13))
    val expr = """ prop["age"] < 13 """
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe Seq(s(0))
  }

  it should " <= filter" in {
    val s = Seq(Person("RU", 12), Person("EN", 13))
    val expr = """ prop["age"] <= 13"""
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe s
  }

  it should " > filter" in {
    val s = Seq(Person("RU", 12), Person("EN", 13))
    val expr = """ prop["age"] > 12 """
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe Seq(s(1))
  }

  it should " >= filter" in {
    val s = Seq(Person("RU", 12), Person("EN", 13))
    val expr = """ prop["age"] >= 12 """
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe s
  }

  it should "be construction simple filter 1" in {
    val s = Seq(Person("RU", 12), Person("EN", 12))
    val expr = """prop["country"] = "RU" && prop["age"] > 10 && prop["age"] < 19 """
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe Seq(s(0))
  }

  it should "be construction simple filter 2" in {
    val s = Seq(Person("RU", 12), Person("EN", 12))
    val expr = """prop["country"] = "RU" && prop["age"] > 10 || prop["age"] < 19 """
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe s
  }


  it should " range filter" in {
    val s = Seq(Person("RU", 12), Person("EN", 13))
    val expr = """prop["age"] < 10 || prop["age"] < 19 && prop["age"] >= 12 """
    val filter = FilterBuilder.buildFilter(expr)
    s.query(filter.get) shouldBe s
  }
}
