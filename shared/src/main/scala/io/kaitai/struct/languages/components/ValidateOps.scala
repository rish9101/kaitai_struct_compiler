package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType.SwitchType
import io.kaitai.struct.datatype.{DataType, KSError, ValidationNotEqualError, ValidationSwitchValueError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, Identifier, IoIdentifier, SwitchValueSpec, ValidationEq, ValidationSpec, ValidationSwitchExpr, YAMLParseException}

import scala.util.{Failure, Success, Try}

/**
  * Common interface for validation operations.
  */
trait ValidateOps extends ExceptionNames {
  def attrValidate(attrId: Identifier, attr: AttrSpec, valid: ValidationSpec): Unit = {
    valid match {
      case ValidationEq(expected) =>
        attrValidateExpr(
          attrId,
          attr.dataType,
          Ast.expr.Compare(
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.cmpop.Eq,
            expected
          ),
          ksErrorName(ValidationNotEqualError(attr.dataType)),
          List(
            expected,
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.expr.Name(IoIdentifier.toAstIdentifier),
            Ast.expr.Str(attr.path.mkString("/", "/", ""))
          )
        )
      case ValidationSwitchExpr(SwitchValueSpec(switchOn, cases, caseElse)) =>
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
    }
  }

  def switchCase(switchOn: Ast.expr, cases: Map[Ast.expr, Ast.expr], caseElse: Ast.expr): Ast.expr = {
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
