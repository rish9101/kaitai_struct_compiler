package io.kaitai.struct

import io.kaitai.struct.datatype.DataType.{CalcIntType, KaitaiStreamType, UserTypeInstream}
import io.kaitai.struct.datatype.{BigEndian, CalcEndian, Endianness, FixedEndian, InheritedEndian, LittleEndian}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.languages.GoCompiler
import io.kaitai.struct.languages.components.ExtraAttrs

class GoClassCompiler(
  classSpecs: ProtocolSpecs,
  override val topClass: ProtocolSpec,
  config: RuntimeConfig
) extends ClassCompiler(classSpecs, topClass, config, GoCompiler) {

  override def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    val extraAttrs = List(
      AttrSpec(List(), IoIdentifier, KaitaiStreamType),
      AttrSpec(List(), RootIdentifier, UserTypeInstream(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    ) ++ ExtraAttrs.forClassSpec(curClass, lang)

    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)

    curClass match {
      case curClass: StructSpec =>
        // Enums declaration defines types, so they need to go first
        compileEnums(curClass)
      case _ =>
    }

    // Basic struct declaration
    lang.classHeader(curClass.name)
    curClass match {
      case curClass: ClassWithSeqSpec =>
        compileAttrDeclarations(curClass.seq ++ extraAttrs)
      case _ =>
    }
    curClass match {
      case curClass: StructSpec =>
        curClass.instances.foreach { case (instName, instSpec) =>
          compileInstanceDeclaration(instName, instSpec)
        }
    }
    lang.classFooter(curClass.name)

    // Constructor = Read() function
    compileReadFunction(curClass)

    curClass match {
      case curClass: StructSpec =>
        compileInstances(curClass)
      case _ =>
    }

    curClass match {
      case curClass: ClassWithSeqSpec =>
        compileAttrReaders(curClass.seq ++ extraAttrs)
      case _ =>
    }

    // Recursive types
    compileSubclasses(curClass)
  }

  def compileReadFunction(curClass: ClassSpec) = {
    val params = curClass match {
      case curClass: StructSpec =>
        curClass.params
      case _ => List()
    }
    lang.classConstructorHeader(
      curClass.name,
      curClass.parentType,
      topClassName,
      curClass.meta.endian.contains(InheritedEndian),
      params
    )
    curClass match {
      case curClass: ClassWithSeqSpec =>
        compileEagerRead(curClass.seq, curClass.meta.endian)
      case _ =>
    }
    lang.classConstructorFooter
  }

  override def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    lang.instanceCheckCacheAndReturn(instName, dataType)

    instSpec match {
      case vi: ValueInstanceSpec =>
        lang.attrParseIfHeader(instName, vi.ifExpr)
        lang.instanceCalculate(instName, dataType, vi.value)
        lang.attrParseIfFooter(vi.ifExpr)
      case i: ParseInstanceSpec =>
        lang.attrParse(i, instName, endian)
    }

    lang.instanceSetCalculated(instName)
    lang.instanceReturn(instName, dataType)
    lang.instanceFooter
  }

  override def compileCalcEndian(ce: CalcEndian): Unit = {
    def renderProc(result: FixedEndian): Unit = {
      val v = result match {
        case LittleEndian => Ast.expr.IntNum(1)
        case BigEndian => Ast.expr.IntNum(0)
      }
      lang.instanceCalculate(IS_LE_ID, CalcIntType, v)
    }
    lang.switchCases[FixedEndian](IS_LE_ID, ce.on, ce.cases, renderProc, renderProc)
  }
}
