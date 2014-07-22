package dsl.expression

import scala.collection.immutable.Stack
import scala.util.parsing.combinator.JavaTokenParsers

object ExpressionBuilder extends JavaTokenParsers {
  def value: Parser[ValueExpr] = atom | integer

  def atom: Parser[ValueExpr] = stringLiteral ^^ { id => ConstStringExpr(id.replace("\"", ""))}

  def integer: Parser[ValueExpr] = wholeNumber ^^ { v => ConstIntExpr(v.toInt)}

  def property: Parser[PropertyExpr] = "prop[" ~ stringLiteral ~ "]" ^^ { case _ ~ prop ~ _ =>
    PropertyExpr(prop.replace("\"", ""))
  }

  def binaryOp: Parser[PropOpExpr] = property ~ ( "<=" | ">=" | "=" | "<" | ">" ) ~ value ^^ {
    case prop ~ op ~ v =>
      op match {
        case "=" => EqualOpExpr(prop, v)
        case "<" => LessOpExpr(prop, v)
        case ">" => GreatOpExpr(prop, v)
        case ">=" => GreatEqOpExpr(prop, v)
        case "<=" => LessEqOpExpr(prop, v)
      }
  }

  def combOp: Parser[Expr] = binaryOp ~ rep(("||" | "&&") ~ binaryOp) ^^ { case first ~ ops =>

    val r = ops.foldLeft(Stack[Expr](first)) {
      case (agg, "&&" ~ rightExp) =>
        val (top, tail) = agg.pop2

        val s = top match {
          case OrOpExpr(left, right) =>
            OrOpExpr(left, AndOpExpr(right, rightExp))
          case AndOpExpr(left, right) =>
            AndOpExpr(left, AndOpExpr(right, rightExp))
          case simple =>
            AndOpExpr(simple, rightExp)
        }
        tail.push(s)

      case (agg, "||" ~ rightExp) =>
        val (top, tail) = agg.pop2

        val s = top match {
          case OrOpExpr(left, right) =>
            OrOpExpr(left, OrOpExpr(right, rightExp))
          case AndOpExpr(left, right) =>
            OrOpExpr(left, AndOpExpr(right, rightExp))
          case simple =>
            OrOpExpr(simple, rightExp)
        }
        tail.push(s)
    }
    rewriteWithRange(r.head)
  }

  def rewriteWithRange(expr: Expr): Expr = {
    expr match {
      case AndOpExpr(first: PropOpExpr, second: PropOpExpr) if first.prop.name == second.prop.name =>
        (first, second) match {
          case (LessOpExpr(p1, maxVal), GreatOpExpr(p2, minVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal))

          case (GreatOpExpr(p2, minVal), LessOpExpr(p1, maxVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal))

          case (GreatEqOpExpr(p2, minVal), LessOpExpr(p1, maxVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal), upperInclusive = true)

          case (GreatOpExpr(p2, minVal), LessEqOpExpr(p1, maxVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal), upperInclusive = true)

          case (LessEqOpExpr(p1, maxVal), GreatOpExpr(p2, minVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal), lowerInclusive = true)

          case (LessOpExpr(p1, maxVal), GreatEqOpExpr(p2, minVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal), lowerInclusive = true)

          case (LessEqOpExpr(p1, maxVal), GreatEqOpExpr(p2, minVal)) =>
            RangeOpExpr(p1, Some(minVal), Some(maxVal), upperInclusive = true, lowerInclusive = true)

          case (f, s) =>
            AndOpExpr(rewriteWithRange(f), rewriteWithRange(s))
        }

      case AndOpExpr(f, s) =>
        AndOpExpr(rewriteWithRange(f), rewriteWithRange(s))

      case OrOpExpr(f, s) =>
        OrOpExpr(rewriteWithRange(f), rewriteWithRange(s))

      case e => e
    }
  }

  def parse(expression: String): Expr = {
    parseAll(combOp, expression).get
  }

}
