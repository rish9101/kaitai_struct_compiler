package io.kaitai.struct.languages.components

import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.DataType._
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

/**
  * Helper trait for languages where single parsing of every standard or user data type is done as expression, i.e. an
  * rvalue. In these languages, "attrStdTypeParse" is replaced with higher-level API: "stdTypeParseExpr" and
  * "handleAssignment".
  */
trait EveryReadIsExpression extends LanguageCompiler {
  def debug = false

  override def attrParse(attr: AttrLikeSpec, id: Identifier, extraAttrs: ListBuffer[AttrSpec], io: String): Unit = {
    if (debug)
      attrDebugStart(id, io, NoRepeat)

    attr.cond.ifExpr match {
      case Some(e) =>
        instanceClear(id)
        condIfHeader(e)
        instanceSetCalculated(id)
      case None => // ignore
    }

    attr.cond.repeat match {
      case RepeatEos =>
        condRepeatEosHeader(id, io, attr.dataType, needRaw(attr.dataType))
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatEosFooter
      case RepeatExpr(repeatExpr: Ast.expr) =>
        condRepeatExprHeader(id, io, attr.dataType, needRaw(attr.dataType), repeatExpr)
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatExprFooter
      case RepeatUntil(untilExpr: Ast.expr) =>
        condRepeatUntilHeader(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
        condRepeatUntilFooter(id, io, attr.dataType, needRaw(attr.dataType), untilExpr)
      case NoRepeat =>
        attrParse2(id, attr.dataType, io, extraAttrs, attr.cond.repeat)
    }

    attr.cond.ifExpr match {
      case Some(e) => condIfFooter(e)
      case None => // ignore
    }

    if (debug)
      attrDebugEnd(id, io, NoRepeat)
  }

  def attrParse2(id: Identifier, dataType: BaseType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    if (debug && rep != NoRepeat)
      attrDebugStart(id, io, rep)

    dataType match {
      case FixedBytesType(c, _) =>
        attrFixedContentsParse(id, c)
      case t: UserType =>
        attrUserTypeParse(id, t, io, extraAttrs, rep)
      case t: BytesType =>
        attrBytesTypeParse(id, t, io, extraAttrs, rep)
      case SwitchType(on, cases) =>
        attrSwitchTypeParse(id, on, cases, io, extraAttrs, rep)
      case _ =>
        val expr = parseExpr(dataType, io)
        handleAssignment(id, expr, rep)
    }

    if (debug && rep != NoRepeat)
      attrDebugEnd(id, io, rep)
  }

  def attrBytesTypeParse(id: Identifier, dataType: BytesType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    // use intermediate variable name, if we'll be doing post-processing
    val rawId = dataType.process match {
      case None => id
      case Some(_) =>
        extraAttrs += AttrSpec(RawIdentifier(id), dataType)
        RawIdentifier(id)
    }

    val expr = parseExpr(dataType, io)
    handleAssignment(rawId, expr, rep)

    // apply post-processing
    dataType.process.foreach((proc) => attrProcess(proc, rawId, id))
  }

  def attrUserTypeParse(id: Identifier, dataType: UserType, io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    val newIO = dataType match {
      case knownSizeType: UserTypeKnownSize =>
        // we have a fixed buffer, thus we shall create separate IO for it
        val rawId = RawIdentifier(id)
        val byteType = knownSizeType match {
          case UserTypeByteLimit(_, size, process) => BytesLimitType(size, process)
          case UserTypeEos(_, process) => BytesEosType(process)
        }

        attrParse2(rawId, byteType, io, extraAttrs, rep)

        val extraType = rep match {
          case NoRepeat => byteType
          case _ => ArrayType(byteType)
        }

        extraAttrs += AttrSpec(rawId, extraType)

        this match {
          case thisStore: AllocateAndStoreIO =>
            val ourIO = thisStore.allocateIO(rawId, rep)
            extraAttrs += AttrSpec(ourIO, KaitaiStreamType)
            privateMemberName(ourIO)
          case thisLocal: AllocateIOLocalVar =>
            thisLocal.allocateIO(rawId, rep)
        }
      case _: UserTypeInstream =>
        // no fixed buffer, just use regular IO
        io
    }
    val expr = parseExpr(dataType, newIO)
    handleAssignment(id, expr, rep)
  }

  def needRaw(dataType: BaseType): Boolean = {
    dataType match {
      case t: UserTypeKnownSize => true
      case _ => false
    }
  }

  val ELSE_CONST = Ast.expr.Name(Ast.identifier("_"))

  def attrSwitchTypeParse(id: Identifier, on: Ast.expr, cases: Map[Ast.expr, BaseType], io: String, extraAttrs: ListBuffer[AttrSpec], rep: RepeatSpec): Unit = {
    switchStart(id, on)

    // Pass 1: only normal case clauses
    var first = true

    cases.foreach { case (condition, dataType) =>
      condition match {
        case ELSE_CONST =>
          // skip for now
        case _ =>
          if (first) {
            switchCaseFirstStart(condition)
            first = false
          } else {
            switchCaseStart(condition)
          }
          attrParse2(id, dataType, io, extraAttrs, rep)
          switchCaseEnd()
      }
    }

    // Pass 2: else clause, if it is there
    cases.foreach { case (condition, dataType) =>
      condition match {
        case ELSE_CONST =>
          switchElseStart()
          attrParse2(id, dataType, io, extraAttrs, rep)
          switchElseEnd()
        case _ =>
          // ignore normal case clauses
      }
    }

    switchEnd()
  }

  def handleAssignment(id: Identifier, expr: String, rep: RepeatSpec): Unit = {
    rep match {
      case RepeatEos => handleAssignmentRepeatEos(id, expr)
      case RepeatExpr(_) => handleAssignmentRepeatExpr(id, expr)
      case RepeatUntil(_) => handleAssignmentRepeatUntil(id, expr)
      case NoRepeat => handleAssignmentSimple(id, expr)
    }
  }

  def attrDebugStart(attrName: Identifier, io: String, repeat: RepeatSpec): Unit = {}
  def attrDebugEnd(attrName: Identifier, io: String, repeat: RepeatSpec): Unit = {}

  def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit
  def handleAssignmentRepeatUntil(id: Identifier, expr: String): Unit
  def handleAssignmentSimple(id: Identifier, expr: String): Unit

  def parseExpr(dataType: BaseType, io: String): String

  def instanceCalculate(instName: InstanceIdentifier, dataType: BaseType, value: Ast.expr) =
    handleAssignmentSimple(instName, expression(value))

  def switchStart(id: Identifier, on: Ast.expr): Unit = ???
  def switchCaseFirstStart(condition: Ast.expr): Unit = switchCaseStart(condition)
  def switchCaseStart(condition: Ast.expr): Unit = ???
  def switchCaseEnd(): Unit = ???
  def switchElseStart(): Unit = ???
  def switchElseEnd(): Unit = switchCaseEnd()
  def switchEnd(): Unit = ???
}
