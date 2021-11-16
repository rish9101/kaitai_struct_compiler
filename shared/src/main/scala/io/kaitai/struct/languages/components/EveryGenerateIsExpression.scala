package io.kaitai.struct.languages.components
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, FixedEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.{expr, typeId}
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.InternalCompilerError

import scala.util.{Failure, Success, Try}

trait EveryGenerateIsExpression extends LanguageCompiler
  with ObjectOrientedLanguage
  with EveryReadIsExpression
  with EveryWriteIsExpression
  with ValidateOps {
  override def attrGenerate(attr: AttrLikeSpec, id: Identifier, valid: Option[ValidationSpec], defEndian: Option[FixedEndian], checkExcludes: Boolean): Unit = {
    val skipIfExcluded: Ast.expr = Ast.expr.UnaryOp(Ast.unaryop.Not,
      Ast.expr.BinOp(
      Ast.expr.Name(ExludesIdentifier.toAstIdentifier),
      Ast.operator.Contains,
      Ast.expr.Str(id.humanReadable)
    ))
    val args = List(skipIfExcluded)
    val newIfExpr = (attr.cond.ifExpr, checkExcludes) match {
      case (Some(expr), true) => Some(Ast.expr.BoolOp(Ast.boolop.And, skipIfExcluded :: expr :: Nil))
      case (None, true) => Some(skipIfExcluded)
      case (_, false) => attr.cond.ifExpr
    }

    attrParseIfHeader(id, newIfExpr)

    val io = normalIO

    attr.cond.repeat match {
      case RepeatEos => {
        val repeatExpr: Ast.expr = Ast.expr.BinOp(Ast.expr.RandInt(None, None), Ast.operator.Add, Ast.expr.IntNum(1))
        val randLen = NamedIdentifier("rand_len")
        handleAssignmentSimple(randLen, translator.translate(repeatExpr))
        condRepeatExprHeader(id, io, attr.dataType, needRaw(attr.dataType), Ast.expr.Name(randLen.toAstIdentifier))
        val repeatSpec = RepeatExpr(Ast.expr.Name(randLen.toAstIdentifier))
        attrGenerate1(id, attr.dataType, io, repeatSpec, false, valid, defEndian)
        condRepeatExprFooter
      }
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, needRaw(attr.dataType), repeatExpr)
        attrGenerate1(id, attr.dataType, io, attr.cond.repeat, false, valid, defEndian)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
        attrGenerate1(id, attr.dataType, io, attr.cond.repeat, false, valid, defEndian)
        condRepeatUntilFooter(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
      case NoRepeat =>
        attrGenerate1(id, attr.dataType, io, attr.cond.repeat, false, valid, defEndian)
    }

    attrParseIfFooter(newIfExpr)
  }

  def attrGenerate1(
    id: Identifier,
    dataType: DataType,
    io: String,
    repeat: RepeatSpec,
    isRaw: Boolean,
    valid: Option[ValidationSpec],
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType] = None
  ) = {
    (valid, dataType) match {
      case (Some(v), t: UserType) =>
        val expr = attrUserTypeGenerate(id, t, io, repeat, isRaw, defEndian, exprTypeOpt)
        handleAssignment(id, expr, repeat, isRaw)
        val excludes = attrGenerateValid(id, dataType, io, repeat, isRaw, v, defEndian)
        userTypeDebugGenerate(generateExprAsString(Ast.expr.Name(id.toAstIdentifier), repeat, isRaw), Some(excludes))
      case (None, t: UserType) =>
        val expr = attrUserTypeGenerate(id, t, io, repeat, isRaw, defEndian, exprTypeOpt)
        handleAssignment(id, expr, repeat, isRaw)
        userTypeDebugGenerate(generateExprAsString(Ast.expr.Name(id.toAstIdentifier), repeat, isRaw))
      case (Some(v), _) =>
        attrGenerateValid(id, dataType, io, repeat, isRaw, v, defEndian)
      case (None, t) => t match {
        case t: BytesType =>
          val expr = attrBytesTypeGenerate(id, t, io, repeat, isRaw)
          handleAssignment(id, expr, repeat, isRaw)
        case st: SwitchType =>
          val isNullable = if (switchBytesOnlyAsRaw) {
            st.isNullableSwitchRaw
          } else {
            st.isNullable
          }
          attrSwitchTypeGenerate(id, st.on, st.cases, io, repeat, defEndian, isNullable, st.combinedType)
        case t: StrFromBytesType =>
          // FIXME generate strings with valid encoding
          val expr = translator.bytesToStr(attrBytesTypeGenerate(id, t, io, repeat, isRaw), Ast.expr.Str(t.encoding))
          handleAssignment(id, expr, repeat, isRaw)
        case t: EnumType =>
          throw new InternalCompilerError("EnumType must have a ValidationSpec and thus not be generated primitively")
        case t: IntType with ReadableType =>
          val expr = attrIntTypeGenerate(id, t, io, repeat, isRaw, defEndian)
          handleAssignment(id, expr, repeat, isRaw)
        case _ =>
          throw new InternalCompilerError("Must have missed something?")
      }
    }
  }

  def attrGenerateValid(
    attrId: Identifier,
    dataType: DataType,
    io: String,
    repeat: RepeatSpec,
    isRaw: Boolean,
    valid: ValidationSpec,
    defEndian: Option[FixedEndian]): List[String] = {

    valid match {
      case ValidationEq(expected) =>
        attrGenerateValid(attrId, dataType, io, repeat, isRaw, ValidationExprEq(Ast.expr.Name(attrId.toAstIdentifier), expected), defEndian)
      case ValidationExprEq(expr, expected) =>
        attrGenerateExpr(
          attrId,
          dataType,
          io,
          repeat,
          isRaw,
          Ast.expr.Compare(
            expr,
            Ast.cmpop.Eq,
            expected
          ),
          defEndian
        )
        List()
      case ValidationSwitchExpr(SwitchValue(switchOn, cases, caseElse)) =>
        attrGenerateExpr(
          attrId,
          dataType,
          io,
          repeat,
          isRaw,
          Ast.expr.Compare(
            Ast.expr.Name(attrId.toAstIdentifier),
            Ast.cmpop.Eq,
            switchCase(switchOn, cases - SwitchType.ELSE_CONST, caseElse)
          ),
          defEndian
        )
        List()
      case ValidationSeqContains(seq) =>
        attrGenerateExpr(
          attrId,
          dataType,
          io,
          repeat,
          isRaw,
          Ast.expr.BinOp(
            Ast.expr.List(seq),
            Ast.operator.Contains,
            Ast.expr.Name(attrId.toAstIdentifier)
          ),
          defEndian
        )
        List()
      case ValidationEnumContains(enum) =>
        val (enumParentClassName: List[String], enumName: String) = enum.enumSpec.get.name match {
          case clsName :+ enumName => (clsName, enumName)
        }
        val enumParentClassId = typeId(absolute = true, enumParentClassName, isArray = false)
        attrGenerateExpr(
          attrId,
          dataType,
          io,
          repeat,
          isRaw,
          Ast.expr.BinOp(
            Ast.expr.List(enum.enumSpec.get.sortedSeq.map(_._1).map{(x: Long) =>
              Ast.expr.EnumById(Ast.identifier(enumName), Ast.expr.IntNum(x), enumParentClassId)}),
            Ast.operator.Contains,
            Ast.expr.Name(attrId.toAstIdentifier)
          ),
          defEndian
        )
        List()
      case ValidationAllOf(cmap) =>
        var fixedArgs: List[String] = List()
        cmap.constraints.foreach { case (expr, expected) =>
          val newExpr = replaceThis(expr, attrId)
          attrGenerateValid(attrId, dataType, io, repeat, isRaw, ValidationExprEq(newExpr, expected), defEndian)
          getFirstAttr(newExpr).foreach((x) => fixedArgs :+= translator.translate(Ast.expr.Str(x)))
        }
        fixedArgs
    }
  }

  def attrGenerateExpr(
    attrId: Identifier,
    attrType: DataType,
    io: String,
    repeat: RepeatSpec,
    isRaw: Boolean,
    calcExpr: Ast.expr,
    defEndian: Option[FixedEndian]
  ): Unit = {
    calcExpr match {
      case Ast.expr.Compare(expr, Ast.cmpop.Eq, expected) =>
        handleAssignmentTempVar(attrType, generateExprAsString(expr, repeat, isRaw), translator.translate(expected))
      case Ast.expr.BinOp(seq, op, value) => op match {
        case Ast.operator.Contains => {
          handleAssignment(attrId, attrSeqItemGenerate(translator.translate(seq)), repeat, isRaw)
        }
      }
    }
  }

  def attrUserTypeGenerate(
    id: Identifier,
    t: UserType,
    io: String,
    repeat: RepeatSpec,
    isRaw: Boolean,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType] = None): String = {

    val exprType = exprTypeOpt.getOrElse(t)
    val expr = generateExprAsString(Ast.expr.Name(id.toAstIdentifier), repeat, isRaw)
    t match {
      case _: UserTypeInstream =>
        attrUserTypeInstreamGenerate(io, expr, t, exprType)
      case knownSizeType: UserTypeFromBytes =>
        val rawId = RawIdentifier(id)
        val byteType = knownSizeType.bytes
        byteType.process match {
          case None => attrUserTypeInstreamGenerate(io, expr, t, exprType)
          case Some(process) =>
            byteType match {
              case blt: BytesLimitType =>
                this match {
                  case thisLocal: AllocateIOLocalVar =>
                    val ioFixed = thisLocal.allocateIOFixed(rawId, translator.translate(blt.size))
                    attrUserTypeInstreamGenerate(ioFixed, expr, t, exprType)
                    handleAssignment(rawId, exprStreamToByteArray(ioFixed), repeat, isRaw)
                    attrBytesTypeGenerate(rawId, byteType, io, repeat, isRaw)
                }
            }
        }
    }
  }

  def attrBytesTypeGenerate(id: Identifier, t: DataType, io: String, repeat: RepeatSpec, isRaw: Boolean): String = {
    // FIXME what's going on with processed fields?
    val idToGenerate = t match {
      case t: BytesType => t.process match {
        case Some(proc) =>
          RawIdentifier(id)
        case _ => id
      }
      case _ => id
    }
    val strEncoding: Option[String] = t match {
      case t: StrFromBytesType => Some(t.encoding)
      case _ => None
    }
    val tt = t match {
      case t: StrFromBytesType => t.bytes
      case _ => t
    }
    tt match {
      case t: BytesEosType =>
//        val expr = generateExprAsString(idToGenerate, repeat, isRaw)
        // FIXME hardcoded randint range
        val size: Ast.expr = Ast.expr.RandInt(Some(1), Some(256))
        attrBytesKnownSizeGenerate(io, t, size, strEncoding)
      case blt: BytesLimitType =>
//        val expr = generateExprAsString(idToGenerate, repeat, isRaw)
        attrBytesKnownSizeGenerate(io, blt, blt.size, strEncoding)
      case t: BytesTerminatedType =>
//        val expr = generateExprAsString(idToGenerate, repeat, isRaw)
        val size: Ast.expr = Ast.expr.RandInt(Some(1), Some(256))
        // FIXME the case where the terminator is not consumed and not included
//        if (!t.consume && !t.include)
//          throw new RuntimeException("I have not programmed this path yet")
        attrBytesKnownSizeGenerate(io, t, size, strEncoding)
    }
  }

  def attrBytesKnownSizeGenerate(io: String, bytesType: BytesType, size: expr, encoding: Option[String] = None): String = {
    val (terminator, pad, include) = bytesType match {
      case limitBytes: BytesLimitType => (limitBytes.terminator, limitBytes.padRight, limitBytes.include)
      case eosBytes: BytesEosType => (eosBytes.terminator, eosBytes.padRight, eosBytes.include)
      case terminatedBytes: BytesTerminatedType => (Some(terminatedBytes.terminator), None, terminatedBytes.include)
    }
    val (term: Option[Int], padRight: Option[Int]) = if (include) {
      (terminator, pad)
    } else {
      (None, pad)
    }
    attrBytesLimitGenerate(io, translator.translate(size), term, padRight, encoding = encoding)
  }

  def attrSwitchTypeGenerate(
    id: Identifier,
    on: expr,
    cases: Map[expr, DataType],
    io: String,
    rep: RepeatSpec,
    defEndian: Option[FixedEndian],
    isNullable: Boolean,
    assignType: DataType
  ): Unit = {
    if (isNullable)
      condIfSetNull(id)

    switchCases[DataType](id, on, cases,
      (dataType) => {
        if (isNullable)
          condIfSetNonNull(id)
        attrGenerate1(id, dataType, io, rep, false, None, defEndian, Some(assignType))
      },
      (dataType) => if (switchBytesOnlyAsRaw) {
        dataType match {
          case t: BytesType =>
            attrGenerate1(RawIdentifier(id), dataType, io, rep, false, None, defEndian, Some(assignType))
          case _ =>
            attrGenerate1(id, dataType, io, rep, false, None, defEndian, Some(assignType))
        }
      } else {
        attrGenerate1(id, dataType, io, rep, false, None, defEndian, Some(assignType))
      }
    )
  }

  def attrIntTypeGenerate(id: Identifier, t: IntType with ReadableType, io: String, repeat: RepeatSpec, isRaw: Boolean, defEndian: Option[FixedEndian]): String = {
    attrIntGenerate(io, t, defEndian): String
  }

  private def generateExprAsExpr(expr: Ast.expr, rep: RepeatSpec, isRaw: Boolean): Ast.expr = {
    expr match {
      case Ast.expr.Name(id) =>
        rep match {
          case NoRepeat =>
            expr
          case _ =>
            Ast.expr.Subscript(
              Ast.expr.Name(Ast.identifier(id.name)),
              Ast.expr.Name(Ast.identifier(Identifier.INDEX))
            )
        }
      case Ast.expr.Attribute(value, attr) =>
        Ast.expr.Attribute(generateExprAsExpr(value, rep, isRaw), attr)
      case Ast.expr.Call(value, attr) =>
        Ast.expr.Call(generateExprAsExpr(value, rep, isRaw), attr)
      case Ast.expr.EnumById(enumType, id, inType) =>
        Ast.expr.EnumById(enumType, generateExprAsExpr(id, rep, isRaw), inType)
      case Ast.expr.UnaryOp(op: Ast.unaryop, inner: Ast.expr) =>
        Ast.expr.UnaryOp(op, generateExprAsExpr(inner, rep, isRaw))
      case Ast.expr.Compare(left: Ast.expr, op: Ast.cmpop, right: Ast.expr) =>
        Ast.expr.Compare(generateExprAsExpr(left, rep, isRaw), op, generateExprAsExpr(right, rep, isRaw))
      case Ast.expr.BinOp(left: Ast.expr, op: Ast.operator, right: Ast.expr) =>
        Ast.expr.BinOp(generateExprAsExpr(left, rep, isRaw), op, generateExprAsExpr(right, rep, isRaw))
      case Ast.expr.BoolOp(op: Ast.boolop, values: Seq[Ast.expr]) =>
        Ast.expr.BoolOp(op, values.map(generateExprAsExpr(_, rep, isRaw)))
      case Ast.expr.IfExp(condition, ifTrue, ifFalse) =>
        Ast.expr.IfExp(generateExprAsExpr(condition, rep, isRaw),
          generateExprAsExpr(ifTrue, rep, isRaw),
          generateExprAsExpr(ifFalse, rep, isRaw))
      case Ast.expr.Subscript(container: Ast.expr, idx: Ast.expr) =>
        Ast.expr.Subscript(
          generateExprAsExpr(container, rep, isRaw),
          generateExprAsExpr(idx, rep, isRaw))
      case Ast.expr.List(values: Seq[Ast.expr]) =>
        Ast.expr.List(values.map(generateExprAsExpr(_, rep, isRaw)))
      case Ast.expr.CastToType(value, typeName) =>
        Ast.expr.CastToType(generateExprAsExpr(value, rep, isRaw), typeName)
      case _ => expr
    }
  }

  /**
    * Must be kept in synchrony with ValidateOps::replaceThis
    */
  private def generateExprAsString(expr: Ast.expr, rep: RepeatSpec, isRaw: Boolean): String =
    translator.translate(generateExprAsExpr(expr, rep, isRaw))

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

  def userTypeDebugGenerate(id: String, excludes: Option[List[String]] = None): Unit
  def attrUserTypeInstreamGenerate(io: String, expr: String, t: UserType, exprType: DataType): String
//  def attrBytesLimitGenerate(io: String, size: String, encoding: Option[String]): String
  def attrBytesLimitGenerate(io: String, size: String, terminator: Option[Int] = None, padRight: Option[Int] = None, encoding: Option[String] = None): String
  def attrIntGenerate(io: String, t: IntType with ReadableType, defEndian: Option[FixedEndian]): String
  def attrSeqItemGenerate(seq: String): String
}
