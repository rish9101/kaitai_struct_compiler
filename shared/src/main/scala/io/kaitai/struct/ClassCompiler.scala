package io.kaitai.struct

import io.kaitai.struct.CompileLog.FileSuccess
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format.{AttrSpec, _}
import io.kaitai.struct.languages.components.{EveryGenerateIsExpression, ExtraAttrs, LanguageCompiler, LanguageCompilerStatic}

class ClassCompiler(
  classSpecs: ProtocolSpecs,
  val topClass: ProtocolSpec,
  config: RuntimeConfig,
  langObj: LanguageCompilerStatic
) extends AbstractCompiler {
  val provider = new ClassTypeProvider(classSpecs, topClass)
  val topClassName = topClass.name
  val lang: LanguageCompiler = langObj.getCompiler(provider, config)

  override def compile: CompileLog.SpecSuccess = {
    lang.fileHeader(topClassName.head)
    compileOpaqueClasses(topClass)
    compileClass(topClass)
    lang.fileFooter(topClassName.head)

    CompileLog.SpecSuccess(
      lang.type2class(topClassName.head),
      lang.results(topClass).map { case (fileName, contents) => FileSuccess(fileName, contents) }.toList
    )
  }

  def compileOpaqueClasses(topClass: ProtocolSpec) = {
    TypeProcessor.getOpaqueClasses(topClass).foreach((classSpec) =>
      if (classSpec != topClass)
        lang.opaqueClassDeclaration(classSpec)
    )
  }

  /**
    * Generates code for one full class using a given [[StructSpec]].
    *
    * @param curClass current class to generate code for
    */
  def compileClass(curClass: ClassSpec): Unit = {
    provider.nowClass = curClass

    if (!lang.innerDocstrings)
      compileClassDoc(curClass)
    lang.classHeader(curClass.name)
    if (lang.innerDocstrings)
      compileClassDoc(curClass)

    // Forward declarations for recursive types
    curClass.types.foreach { case (typeName, _) => lang.classForwardDeclaration(List(typeName)) }

    curClass match {
      case curClass: StructSpec =>
        if (lang.innerEnums) {
          compileEnums(curClass)
        }
      case _ =>
    }

    curClass match {
      case curClass: ClassWithSeqSpec =>
        if (lang.config.readStoresPos) {
          lang.debugClassSequence(curClass.seq)
        }
      case _ =>
    }

    // Constructor
    compileConstructor(curClass)

    curClass match {
      case curClass: ProtocolSpec =>
        // compile ProtocolSpec body
        compileStateMachine(curClass.stateMachine, curClass.meta.endian)
      case curClass: InputSpec =>
        // compile InputSpec body
        compileIterate(curClass.seq, curClass.meta.endian)
      case curClass: ClassWithSeqSpec =>
        // Read method(s)
        compileEagerRead(curClass.seq, curClass.meta.endian)
        if (config.readWrite) {
          compileWrite(curClass.seq, curClass.meta.endian)
          compileGenerate(curClass.seq, curClass.meta.endian)
          // compileCheck(curClass.seq)
        }
      case _ =>
    }

    // Destructor
    compileDestructor(curClass)

    // Recursive types
    if (lang.innerClasses) {
      compileSubclasses(curClass)

      provider.nowClass = curClass
    }

    curClass match {
      case curClass: StructSpec =>
        compileInstances(curClass)
      case _ =>
    }

    // Attributes declarations and readers
    var allAttrs: List[MemberSpec] = List(
      AttrSpec(List(), RootIdentifier, CalcUserType(topClassName, None)),
      AttrSpec(List(), ParentIdentifier, curClass.parentType)
    )

    curClass match {
      case curClass: ClassWithSeqSpec =>
        allAttrs ++= curClass.seq
      case _ =>
    }
    curClass match {
      case curClass: StructSpec =>
        allAttrs ++= curClass.params
      case _ =>
    }
    allAttrs ++= ExtraAttrs.forClassSpec(curClass, lang)

    compileAttrDeclarations(allAttrs)
    compileAttrReaders(allAttrs)

    lang.classFooter(curClass.name)

    if (!lang.innerClasses) {
      compileSubclasses(curClass)
    }

    curClass match {
      case curClass: StructSpec =>
        if (!lang.innerEnums) {
          compileEnums(curClass)
        }
      case _ =>
    }
  }

  /**
    * Compiles constructor for a given class. Generally, it should:
    *
    * * store passed parameters, io/root/parent/endianness if needed
    * * initialize everything
    * * invoke _read() method, if applicable
    *
    * @param curClass current class to generate code for
    */
  def compileConstructor(curClass: ClassSpec) = {
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
    compileInit(curClass)
    curClass match {
      case curClass: StructSpec =>
        curClass.instances.foreach { case (instName, _) => lang.instanceClear(instName) }
      case _ =>
    }
    if (lang.config.autoRead)
      lang.runRead()
    lang.classConstructorFooter
  }

  /**
    * Compile initialization of class members for a given type. Typically
    * this is only required for languages which both:
    *
    * * don't perform auto-initialization of object with some default
    *   values (like 0s) on object creation,
    * * require these members to be initialized because any other
    *   procedures with object (e.g. destruction) will require that
    *
    * Currently, this is only applicable to C++ without smart pointers,
    * as destructors we'll generate will rely on pointers being set to
    * null.
    * @param curClass current type to generate code for
    */
  def compileInit(curClass: ClassSpec) = {
    curClass.variables.foreach { case (varName, varSpec) =>
      varSpec.value match {
        case Some(varExpr) =>
          lang.varInit(varName, varSpec.dataType, varExpr)
        case None =>
      }
    }
    curClass match {
      case curClass: ClassWithSeqSpec =>
        curClass.seq.foreach((attr) => compileAttrInit(attr))
      case _ =>
    }
    curClass match {
      case curClass: StructSpec =>
        curClass.instances.foreach { case (_, instSpec) =>
          instSpec match {
            case pis: ParseInstanceSpec => compileAttrInit(pis)
            case _: ValueInstanceSpec => // ignore for now
          }
        }
      case _ =>
    }
  }

  def compileAttrInit(originalAttr: AttrLikeSpec): Unit = {
    val extraAttrs = ExtraAttrs.forAttr(originalAttr, lang)
    val allAttrs = List(originalAttr) ++ extraAttrs
    allAttrs.foreach((attr) => lang.attrInit(attr))
  }

  /**
    * Compiles destructor for a given type. It should clean up everything
    * (i.e. every applicable allocated seq / instance attribute variables, and
    * any extra attribute variables, if they were used).
    * @param curClass current type to generate code for
    */
  def compileDestructor(curClass: ClassSpec) = {
    lang.classDestructorHeader(curClass.name, curClass.parentType, topClassName)
    curClass match {
      case curClass: ClassWithSeqSpec =>
        curClass.seq.foreach((attr) => lang.attrDestructor(attr, attr.id))
      case _ =>
    }
    curClass match {
      case curClass: StructSpec =>
        curClass.instances.foreach { case (id, instSpec) =>
          instSpec match {
            case pis: ParseInstanceSpec => lang.attrDestructor(pis, id)
            case _: ValueInstanceSpec => // ignore for now
          }
        }
      case _ =>
    }
    lang.classDestructorFooter
  }

  /**
    * Iterates over a given list of attributes and generates attribute
    * declarations for each of them.
    * @param attrs attribute list to traverse
    */
  def compileAttrDeclarations(attrs: List[MemberSpec]): Unit = {
    attrs.foreach { (attr) =>
      val isNullable = if (lang.switchBytesOnlyAsRaw) {
        attr.isNullableSwitchRaw
      } else {
        attr.isNullable
      }
      lang.attributeDeclaration(attr.id, attr.dataTypeComposite, isNullable)
    }
  }

  /**
    * Iterates over a given list of attributes and generates attribute
    * readers (AKA getters) for each of them.
    * @param attrs attribute list to traverse
    */
  def compileAttrReaders(attrs: List[MemberSpec]): Unit = {
    attrs.foreach { (attr) =>
      // FIXME: Python should have some form of attribute docs too
      if (!attr.doc.isEmpty && !lang.innerDocstrings)
        lang.attributeDoc(attr.id, attr.doc)
      val isNullable = if (lang.switchBytesOnlyAsRaw) {
        attr.isNullableSwitchRaw
      } else {
        attr.isNullable
      }
      lang.attributeReader(attr.id, attr.dataTypeComposite, isNullable)
    }
  }

  /**
    * Compiles everything related to "eager reading" for a given list of
    * sequence attributes and endianness. Depending on endianness:
    *
    * * For types known to have fixed endianness, we do just "_read" method.
    * * For types with ambiguous endianness, we'll do `_read` + "_read_le" +
    *   "_read_be" methods. If endianness needs to be calculated, we'll perform
    *   that calculation in "_read". If it's inherited, then we'll just make
    *   decision based on that inherited setting.
    *
    * @param seq list of sequence attributes
    * @param endian endianness setting
    */
  def compileEagerRead(seq: List[AttrLikeSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqReadProc(seq, None)
      case Some(ce: CalcEndian) =>
        lang.readHeader(None, false)
        compileCalcEndian(ce)
        lang.runReadCalc()
        lang.readFooter()

        compileSeqReadProc(seq, Some(LittleEndian))
        compileSeqReadProc(seq, Some(BigEndian))
      case Some(InheritedEndian) =>
        lang.readHeader(None, false)
        lang.runReadCalc()
        lang.readFooter()

        compileSeqReadProc(seq, Some(LittleEndian))
        compileSeqReadProc(seq, Some(BigEndian))
    }
  }

  def compileWrite(seq: List[AttrLikeSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqWriteProc(seq, None)
      case Some(CalcEndian(_, _)) | Some(InheritedEndian) =>
        lang.writeHeader(None)
        lang.runWriteCalc()
        lang.writeFooter()

        compileSeqWriteProc(seq, Some(LittleEndian))
        compileSeqWriteProc(seq, Some(BigEndian))
    }
  }

  def compileGenerate(seq: List[AttrLikeSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqGenerateProc(seq, None)
    }
  }

  def compileCheck(seq: List[AttrLikeSpec]): Unit = {
    lang.checkHeader()
    compileSeqCheck(seq)
    lang.checkFooter()
  }

  def compileIterate(seq: List[InteractionSpec], endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileSeqIterateProc(seq, None)
    }
  }

  def compileStateMachine(stateMachine: StateMachineSpec, endian: Option[Endianness]): Unit = {
    endian match {
      case None | Some(_: FixedEndian) =>
        compileStateMachineProc(stateMachine, None)
    }
  }

  val IS_LE_ID = SpecialIdentifier("_is_le")

  /**
    * Compiles endianness calculation procedure and stores result in a special
    * attribute [[IS_LE_ID]]. Typically occurs as part of "_read" method.
    * @param ce calculated endianness specification
    */
  def compileCalcEndian(ce: CalcEndian): Unit = {
    def renderProc(result: FixedEndian): Unit = {
      val v = Ast.expr.Bool(result == LittleEndian)
      lang.instanceCalculate(IS_LE_ID, CalcBooleanType, v)
    }

    lang.switchCases[FixedEndian](IS_LE_ID, ce.on, ce.cases, renderProc, renderProc)
  }

  /**
    * Compiles seq reading method (complete with header and footer).
    * @param seq sequence of attributes
    * @param defEndian default endianness
    */
  def compileSeqReadProc(seq: List[AttrLikeSpec], defEndian: Option[FixedEndian]) = {
    lang.readHeader(defEndian, seq.isEmpty)
    compileSeqRead(seq, defEndian)
    lang.readFooter()
  }

  def compileSeqWriteProc(seq: List[AttrLikeSpec], defEndian: Option[FixedEndian]) = {
    lang.writeHeader(defEndian)
    compileSeqWrite(seq, defEndian)
    lang.writeFooter()
  }

  def compileSeqGenerateProc(seq: List[AttrLikeSpec], defEndian: Option[FixedEndian]) = {
    lang.generateHeader(defEndian)
    compileSeqGenerate(seq, defEndian)
    lang.generateFooter()
  }

  def compileSeqIterateProc(seq: List[InteractionSpec], defEndian: Option[FixedEndian]) = {
    lang.iterateHeader(defEndian)
    compileSeqIterate(seq, defEndian)
    lang.iterateFooter()
  }

  def compileStateMachineProc(stateMachine: StateMachineSpec, defEndian: Option[FixedEndian]) = {
    lang.stateMachineHeader(defEndian)
    lang.stateMachineDefine(stateMachine)
    lang.stateMachineFooter()
  }

  /**
    * Compiles seq reading method body (only reading statements).
    * @param seq sequence of attributes
    * @param defEndian default endianness
    */
  def compileSeqRead(seq: List[AttrLikeSpec], defEndian: Option[FixedEndian]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attrParse(attr, attr.id, defEndian)
      attr match {
        case attr: ExportSpec =>
          attr.exports match {
            case Some(exports) =>
              lang match {
                case lang: EveryGenerateIsExpression =>
                  lang.attrGenerateValid(attr.id, attr.dataType, lang.normalIO, NoRepeat, false, exports, defEndian)
              }
            case None =>
          }
        case _ =>
      }
//      compileAttrValidate(attr)
      wasUnaligned = nowUnaligned
    }
  }

  def compileSeqWrite(seq: List[AttrLikeSpec], defEndian: Option[FixedEndian]) = {
    var wasUnaligned = false
    seq.foreach { (attr) =>
      val nowUnaligned = isUnalignedBits(attr.dataType)
      if (wasUnaligned && !nowUnaligned)
        lang.alignToByte(lang.normalIO)
      lang.attrWrite(attr, attr.id, defEndian)
      wasUnaligned = nowUnaligned
    }
  }

  def compileSeqGenerate(seq: List[AttrLikeSpec], defEndian: Option[FixedEndian]) = {
    seq.foreach { (attr) =>
      lang.attrGenerate(attr, attr.id, attr.valid, defEndian, true)
    }
  }

  def compileSeqCheck(seq: List[AttrLikeSpec]) = {
    seq.foreach { (attr) =>
      lang.attrCheck(attr, attr.id)
    }
  }

  def compileSeqIterate(seq: List[InteractionSpec], defEndian: Option[FixedEndian]) = {
    seq.foreach { (attr) =>
      attr.action match {
        case TransmitInteraction =>
          lang.attrTransmit(attr, attr.id, attr.valid, defEndian)
        case ReceiveInteraction =>
          lang.attrReceive(attr, attr.id, attr.valid, defEndian)
        case DelayInteraction(delay) =>
          lang.attrDelay(attr, attr.id, delay, attr.valid, defEndian)
      }
      attr.exports match {
        case Some(exports) =>
          lang match {
            case lang: EveryGenerateIsExpression =>
              lang.attrGenerateValid(attr.id, attr.dataType, lang.normalIO, NoRepeat, false, exports, defEndian)
          }
        case None =>
      }
    }
  }

  /**
    * Compiles validation procedure for one attribute after it was parsed.
    * @param attr attribute to validate
    */
  def compileAttrValidate(attr: AttrSpec): Unit =
    attr.valid.foreach(valid => lang.attrValidate(attr.id, attr, valid))

  /**
    * Compiles all enums specifications for a given type.
    * @param curClass current type to generate code for
    */
  def compileEnums(curClass: StructSpec): Unit =
    curClass.enums.foreach { case(_, enumColl) => compileEnum(curClass, enumColl) }

  /**
    * Compile subclasses for a given class.
    * @param curClass current type to generate code for
    */
  def compileSubclasses(curClass: ClassSpec): Unit = {
    curClass.forEach(compileClass)
  }

  def compileInstances(curClass: StructSpec) = {
    curClass.instances.foreach { case (instName, instSpec) =>
      compileInstance(curClass.name, instName, instSpec, curClass.meta.endian)
    }
  }

  def compileInstance(className: List[String], instName: InstanceIdentifier, instSpec: InstanceSpec, endian: Option[Endianness]): Unit = {
    // Determine datatype
    val dataType = instSpec.dataTypeComposite

    compileInstanceDeclaration(instName, instSpec)

    if (!lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
    lang.instanceHeader(className, instName, dataType, instSpec.isNullable)
    if (lang.innerDocstrings)
      compileInstanceDoc(instName, instSpec)
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

  def compileInstanceDeclaration(instName: InstanceIdentifier, instSpec: InstanceSpec): Unit =
    lang.instanceDeclaration(instName, instSpec.dataTypeComposite, instSpec.isNullable)

  def compileEnum(curClass: StructSpec, enumColl: EnumSpec): Unit =
    lang.enumDeclaration(curClass.name, enumColl.name.last, enumColl.sortedSeq)

  def isUnalignedBits(dt: DataType): Boolean =
    dt match {
      case _: BitsType | BitsType1 => true
      case et: EnumType => isUnalignedBits(et.basedOn)
      case _ => false
    }

  def compileClassDoc(curClass: ClassSpec) = {
    if (!curClass.doc.isEmpty)
      lang.classDoc(curClass.name, curClass.doc)
  }

  def compileInstanceDoc(instName: Identifier, instSpec: InstanceSpec) {
    if (!instSpec.doc.isEmpty)
      lang.attributeDoc(instName, instSpec.doc)
  }
}
