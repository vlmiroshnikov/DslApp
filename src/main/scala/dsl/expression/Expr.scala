package dsl.expression

trait Expr {
}

case class ConstIntExpr(value: Int) extends ValueExpr

case class ConstStringExpr(value: String) extends ValueExpr

case class PropertyExpr(name: String) extends Expr

trait PropOpExpr extends Expr {
  def prop: PropertyExpr
}

case class EqualOpExpr(prop: PropertyExpr, value: ValueExpr) extends PropOpExpr

case class GreatOpExpr(prop: PropertyExpr, value: ValueExpr) extends PropOpExpr

case class GreatEqOpExpr(prop: PropertyExpr, value: ValueExpr) extends PropOpExpr

case class LessEqOpExpr(prop: PropertyExpr, value: ValueExpr) extends PropOpExpr

case class LessOpExpr(prop: PropertyExpr, value: ValueExpr) extends PropOpExpr

case class AndOpExpr(left: Expr, right: Expr) extends Expr

case class OrOpExpr(left: Expr, right: Expr) extends Expr

case class RangeOpExpr(prop: PropertyExpr,
                       min: Option[ValueExpr] = None,
                       max: Option[ValueExpr] = None,
                       lowerInclusive :Boolean = false,
                       upperInclusive :Boolean = false) extends PropOpExpr
