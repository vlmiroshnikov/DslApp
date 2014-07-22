package dsl.filter

import dsl.expression.ExpressionBuilder

import scala.util.Try

object FilterBuilder {
  def buildFilter(exp: String): Try[Filter] = Try {
    new Evaluator(ExpressionBuilder.parse(exp))
  }
}
