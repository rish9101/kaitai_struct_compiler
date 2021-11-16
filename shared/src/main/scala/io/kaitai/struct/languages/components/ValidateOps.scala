package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.datatype.{DataType, KSError, ValidationNotEqualError, ValidationSeqContainsError, ValidationSwitchValueError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrLikeSpec, Identifier, IoIdentifier, SelfIdentifier, ValidationAllOf, ValidationEnumContains, ValidationEq, ValidationExprEq, ValidationSeqContains, ValidationSpec, ValidationSwitchExpr, YAMLParseException}
import io.kaitai.struct.datatype.DataType.SwitchValue
import io.kaitai.struct.exprlang.Ast.typeId

import scala.util.{Failure, Success, Try}

/**
  * Common interface for validation operations.
  */
trait ValidateOps extends ExceptionNames {
  def attrValidate(attrId: Identifier, attr: AttrLikeSpec, valid: ValidationSpec): Unit = {
    valid match {
      case ValidationEq(expected) =>
        attrValidate(attrId, attr, ValidationExprEq(Ast.expr.Name(attrId.toAstIdentifier), expected))
      case ValidationExprEq(expr, expected) =>
        attrValidateExpr(
          attrId,
          attr.dataType,
          Ast.expr.Compare(
            expr,
            Ast.cmpop.Eq,
            expected
          ),
          ksErrorName(ValidationNotEqualError(attr.dataType)),
          List(
            expected,
            expr,
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationSwitchExpr(SwitchValue(switchOn, cases, caseElse)) =>
        attrValidateExpr(
          attrId,
          attr.dataType,
          Ast.expr.Compare(
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.cmpop.Eq,
            switchCase(switchOn, cases - SwitchType.ELSE_CONST, caseElse)
          ),
          ksErrorName(ValidationSwitchValueError(attr.dataType)),
          List(
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationSeqContains(seq) =>
        attrValidateExpr(
          attrId,
          attr.dataType,
          Ast.expr.BinOp(
            Ast.expr.List(seq),
            Ast.operator.Contains,
            Ast.expr.Name(attrId.toAstIdentifier)
          ),
          ksErrorName(ValidationSeqContainsError(attr.dataType)),
          List(
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationEnumContains(enum) =>
        val (enumParentClassName: List[String], enumName: String) = enum.enumSpec.get.name match {
          case clsName :+ enumName => (clsName, enumName)
        }
        val enumParentClassId = typeId(absolute = true, enumParentClassName, isArray = false)
        attrValidateExpr(
          attrId,
          attr.dataType,
          Ast.expr.BinOp(
            Ast.expr.List(enum.enumSpec.get.sortedSeq.map(_._1).map{(x: Long) =>
              Ast.expr.EnumById(Ast.identifier(enumName), Ast.expr.IntNum(x), enumParentClassId)}),
            Ast.operator.Contains,
            Ast.expr.Name(attrId.toAstIdentifier)
          ),
          ksErrorName(ValidationSeqContainsError(attr.dataType)),
          List(
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationAllOf(cmap) =>
        cmap.constraints.foreach { case (expr, expected) =>
          val newExpr = replaceThis(expr, attrId)
          attrValidate(attrId, attr, ValidationExprEq(newExpr, expected))
        }
    }
  }

  /**
    * Must be kept in synchrony with EveryGenerateIsExpression::generateExprAsString
    */
  def replaceThis(expr: Ast.expr, attrId: Identifier): Ast.expr = {
    val thisId = SelfIdentifier.toAstIdentifier
    expr match {
      case Ast.expr.Attribute(value, attr) =>
        Ast.expr.Attribute(replaceThis(value, attrId), attr)
      case Ast.expr.Name(`thisId`) =>
        Ast.expr.Name(attrId.toAstIdentifier)
      case _ => expr // throw new RuntimeException("Constraint too complex to parse :(")
    }
  }

  private def switchCase(switchOn: Ast.expr, cases: Map[Ast.expr, Ast.expr], caseElse: Ast.expr): Ast.expr = {
    Try(cases.head) match {
      case Success((caseExpr, valExpr)) =>
        Ast.expr.IfExp(
          Ast.expr.Compare(
            switchOn,
            Ast.cmpop.Eq,
            caseExpr
          ),
          valExpr,
          switchCase(switchOn, cases - caseExpr, caseElse)
        )
      case Failure(_) =>
        caseElse
    }
  }

  def attrValidateExpr(attrId: Identifier, attrType: DataType, checkExpr: Ast.expr, errName: String, errArgs: List[Ast.expr]): Unit = {}
}
