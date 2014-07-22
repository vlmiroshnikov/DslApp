package dsl.filter

import dsl.expression._


class Evaluator(expr: Expr) extends Filter {
  val func = acceptImplF(expr)

  def accept(item: Item): Boolean = {
    val r = func(item)
    r
  }

  type AcceptF = Item => Boolean

  private def createAndFilter(f1: AcceptF, f2: AcceptF): AcceptF = item => f1(item) && f2(item)

  private def createOrFilter(f1: AcceptF, f2: AcceptF): AcceptF = item => f1(item) || f2(item)

  private def acceptImplF(exp: Expr): AcceptF = {
    val s = exp match {
      case EqualOpExpr(PropertyExpr(name), value) =>
        value match {
          case ConstIntExpr(v) => cmp[Int](name, x => x == v)
          case ConstStringExpr(v) => cmp[String](name, x => x.equals(v))
        }

      case LessOpExpr(PropertyExpr(name), value) =>
        value match {
          case ConstIntExpr(v) => cmp[Int](name, x => x < v)
          case ConstStringExpr(v) => cmp[String](name, x => x < v)
        }

      case LessEqOpExpr(PropertyExpr(name), value) =>
        value match {
          case ConstIntExpr(v) => cmp[Int](name, x => x <= v)
          case ConstStringExpr(v) => cmp[String](name, x => x <= v)
        }

      case GreatOpExpr(PropertyExpr(name), value) =>
        value match {
          case ConstIntExpr(v) => cmp[Int](name, x => x > v)
          case ConstStringExpr(v) => cmp[String](name, x => x > v)
        }

      case GreatEqOpExpr(PropertyExpr(name), value) =>
        value match {
          case ConstIntExpr(v) => cmp[Int](name, x => x >= v)
          case ConstStringExpr(v) => cmp[String](name, x => x >= v)
        }

      case RangeOpExpr(PropertyExpr(name), minOpt, maxOpt, lowInc, upInc) =>
        inRangeF(name, minOpt, maxOpt, lowInc, upInc)

      case AndOpExpr(left, right) => createAndFilter(acceptImplF(left), acceptImplF(right))
      case OrOpExpr(left, right) => createOrFilter(acceptImplF(left), acceptImplF(right))
    }
    s
  }

  private def cmp[T](name: String, cmpF: (T) => Boolean): AcceptF = {
    item => item.prop(name) match {
      case Some(v) => cmpF(v.asInstanceOf[T])
      case _ => false
    }
  }

  private def inRangeF(name: String, minOpt: Option[ValueExpr], maxOpt: Option[ValueExpr], lowInc: Boolean, upInc: Boolean): AcceptF = {
    item =>
      (minOpt, maxOpt) match {
        case (Some(ConstIntExpr(min)), Some(ConstIntExpr(max))) =>
          item.prop(name) match {
            case Some(v: Int) =>
              (if (lowInc) min <= v else min < v) && (if (upInc) v <= max else v < max)
            case _ => false
          }

        case (None, Some(ConstIntExpr(max))) =>
          item.prop(name) match {
            case Some(v: Int) => if (upInc) v <= max else v < max
            case _ => false
          }

        case (Some(ConstIntExpr(min)), None) =>
          item.prop(name) match {
            case Some(v: Int) => if (lowInc) min <= v else min < v
            case _ => false
          }
      }
  }
}







