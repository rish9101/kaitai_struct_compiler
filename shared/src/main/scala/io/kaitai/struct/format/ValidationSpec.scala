package io.kaitai.struct.format

import io.kaitai.struct.exprlang.{Ast, Expressions}
import io.kaitai.struct.datatype.DataType.{EnumType, SwitchValue, ConstraintMap}

sealed trait ValidationSpec

case class ValidationEq(value: Ast.expr) extends ValidationSpec
case class ValidationExprEq(expr: Ast.expr, expected: Ast.expr) extends ValidationSpec
case class ValidationRange(min: Option[Ast.expr], max: Option[Ast.expr]) extends ValidationSpec
case class ValidationAnyOf(values: List[Ast.expr]) extends ValidationSpec
case class ValidationExpr(checkExpr: Ast.expr) extends ValidationSpec
case class ValidationSwitchExpr(switchval: SwitchValue) extends ValidationSpec
case class ValidationSeqContains(seq: Seq[Ast.expr]) extends ValidationSpec
case class ValidationEnumContains(enum: EnumType) extends ValidationSpec
case class ValidationAllOf(cmap: ConstraintMap) extends ValidationSpec

object ValidationSpec {
  def fromYaml(src: Any, path: List[String]): ValidationSpec = {
    src match {
      case value: String =>
        fromString(value, path)
      case x: Boolean =>
        fromString(x.toString, path)
      case x: Int =>
        fromString(x.toString, path)
      case x: Long =>
        fromString(x.toString, path)
//      case srcMap: Map[Any, Any] =>
//        fromMap(ParseUtils.anyMapToStrMap(srcMap, path), path)
      case _ =>
        throw YAMLParseException.badType("string or map", src, path)
    }
  }

  def fromString(value: String, path: List[String]): ValidationSpec =
    ValidationEq(Expressions.parse(value))
}
