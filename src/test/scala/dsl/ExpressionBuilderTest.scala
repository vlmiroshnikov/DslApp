package dsl

import dsl.expression.ExpressionBuilder
import org.scalatest.{FlatSpec, Matchers}

class ExpressionBuilderTest extends FlatSpec with Matchers {
  it should "be parse simple expression" in {
    val expr = """prop["country"] = "RU" || prop["age"] > 10 && prop["age"] < 19  || prop["state"] < 1"""
    ExpressionBuilder.parse(expr) match {
      case v => println(v)
    }
  }
}
