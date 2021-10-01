package io.kaitai.struct.languages

import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.{DataType, EndOfStreamError, FixedEndian, InheritedEndian, KSError, UndecidedEndiannessError}
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._
import io.kaitai.struct.languages.components._
import io.kaitai.struct.translators.PythonTranslator
import io.kaitai.struct.{ClassTypeProvider, RuntimeConfig, StringLanguageOutputWriter, Utils}

class PythonCompiler(typeProvider: ClassTypeProvider, config: RuntimeConfig)
  extends LanguageCompiler(typeProvider, config)
    with ObjectOrientedLanguage
    with UpperCamelCaseClasses
    with SingleOutputFile
    with UniversalFooter
    with EveryReadIsExpression
    with EveryWriteIsExpression
    with EveryInitIsExpression
    with AllocateIOLocalVar
    with FixedContentsUsingArrayByteLiteral
    with UniversalDoc
    with NoNeedForFullClassPath {

  import PythonCompiler._

  override val translator = new PythonTranslator(typeProvider, importList)

  override def innerDocstrings = true

  override def universalFooter: Unit = {
    out.dec
    out.puts
  }

  override def indent: String = "    "
  override def outFileName(topClassName: String): String = s"$topClassName.py"

  override def outImports(topClass: ClassSpec) =
    importList.toList.mkString("", "\n", "\n")

  override def fileHeader(topClassName: String): Unit = {
    outHeader.puts(s"# $headerComment")
    outHeader.puts

    importList.add("from pkg_resources import parse_version")
    importList.add("import kaitaistruct")
    importList.add(s"from kaitaistruct import $kstructName, $kstreamName, $kfieldName, BytesIO")

    out.puts
    out.puts

    // API compatibility check
    out.puts(
      "if parse_version(kaitaistruct.__version__) < parse_version('" +
        KSVersion.minimalRuntime +
        "'):"
    )
    out.inc
    out.puts(
      "raise Exception(\"Incompatible Kaitai Struct Python API: " +
        KSVersion.minimalRuntime +
        " or later is required, but you have %s\" % (kaitaistruct.__version__))"
    )
    out.dec
    out.puts

    out.puts("exports = dict()")

  }

  override def opaqueClassDeclaration(classSpec: ClassSpec): Unit = {
    val name = classSpec.name.head
    out.puts(
      if (config.pythonPackage.nonEmpty) {
        s"from ${config.pythonPackage} import $name"
      } else {
        s"import $name"
      }
    )
  }

  override def classHeader(name: String): Unit = {
    out.puts(s"class ${type2class(name)}($kstructName):")
    out.inc
  }

  override def classHeaderInput(name: List[String]): Unit = {
    out.puts(s"class ${type2class(name.last)}($kstructName, GrammarInput):")
    out.inc
  }

  override def classConstructorHeader(name: String, parentType: DataType, rootClassName: String, isHybrid: Boolean, params: List[ParamDefSpec]): Unit = {
    val endianAdd = if (isHybrid) ", _is_le=None" else ""
    val paramsList = Utils.join(params.map((p) => paramName(p.id)), ", ", ", ", "")

    out.puts(s"def __init__(self$paramsList, _io, _parent=None, _root=None$endianAdd):")
    out.inc
    out.puts("self._io = _io")
    out.puts("self._parent = _parent")
    out.puts("self._root = _root if _root else self")
    out.puts("self._fields_init()")

    if (isHybrid)
      out.puts("self._is_le = _is_le")

    // Store parameters passed to us
    params.foreach((p) => handleAssignmentSimple(p.id, paramName(p.id)))

    if (config.readStoresPos) {
      importList.add("import collections")
      out.puts("self._debug = collections.defaultdict(dict)")
    }
  }

  override def runRead(): Unit = {
    out.puts("self._read()")
  }

  override def runReadCalc(): Unit = {
    out.puts(s"if not hasattr(self, '_is_le'):")
    out.inc
    out.puts(s"raise ${ksErrorName(UndecidedEndiannessError)}(" + "\"" + typeProvider.nowClass.path.mkString("/", "/", "") + "\")")
    out.dec
    out.puts(s"elif self._is_le == True:")
    out.inc
    out.puts("self._read_le()")
    out.dec
    out.puts("elif self._is_le == False:")
    out.inc
    out.puts("self._read_be()")
    out.dec
  }

  override def readHeader(endian: Option[FixedEndian], isEmpty: Boolean): Unit = {
    val suffix = endian match {
      case Some(e) => s"_${e.toSuffix}"
      case None => ""
    }
    out.puts(s"def _read$suffix(self):")
    out.inc
    if (isEmpty)
      out.puts("pass")
  }

  override def writeHeader(endian: Option[FixedEndian]): Unit = {
    val suffix = endian match {
      case Some(e) => s"${e.toSuffix}"
      case None => ""
    }
    out.puts(s"def _write$suffix(self):")
    out.inc
  }
  
  override def initHeader(): Unit = {
    out.puts(s"def _fields_init(self):")
    out.inc
    out.puts(s"self._fields = list()")
  }
  
  override def readFooter() = universalFooter

  override def initFooter() = universalFooter

  override def attributeDeclaration(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def attributeReader(attrName: Identifier, attrType: DataType, isNullable: Boolean): Unit = {}

  override def universalDoc(doc: DocSpec): Unit = {
    val docStr = doc.summary match {
      case Some(summary) =>
        val lastChar = summary.last
        if (lastChar == '.' || lastChar == '\n') {
          summary
        } else {
          summary + "."
        }
      case None =>
        ""
    }

    val extraNewline = if (docStr.isEmpty || docStr.last == '\n') "" else "\n"
    val refStr = doc.ref.map {
      case TextRef(text) =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", text)
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
      case ref: UrlRef =>
        val seeAlso = new StringLanguageOutputWriter("")
        seeAlso.putsLines("   ", s"${ref.text} - ${ref.url}")
        s"$extraNewline\n.. seealso::\n${seeAlso.result}"
    }.mkString("\n")

    out.putsLines("", "\"\"\"" + docStr + refStr + "\"\"\"")
  }

  override def attrFixedContentsParse(attrName: Identifier, contents: String): Unit =
    out.puts(s"${privateMemberName(attrName)} = self._io.ensure_fixed_contents($contents)")

  override def attrParseHybrid(leProc: () => Unit, beProc: () => Unit): Unit = {
    out.puts("if self._is_le:")
    out.inc
    leProc()
    out.dec
    out.puts("else:")
    out.inc
    beProc()
    out.dec
  }

  override def attrProcess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        val procName = translator.detectType(xorValue) match {
          case _: IntType => "process_xor_one"
          case _: BytesType => "process_xor_many"
        }
        out.puts(s"$destName = $kstreamName.$procName($srcName, ${expression(xorValue)})")
      case ProcessZlib =>
        importList.add("import zlib")
        out.puts(s"$destName = zlib.decompress($srcName)")
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.process_rotate_left($srcName, $expr, 1)")
      case ProcessCustom(name, args) =>
        val procClass = if (name.length == 1) {
          val onlyName = name.head
          val className = type2class(onlyName)
          importList.add(s"from $onlyName import $className")
          className
        } else {
          val pkgName = name.init.mkString(".")
          importList.add(s"import $pkgName")
          s"$pkgName.${type2class(name.last)}"
        }

        out.puts(s"_process = $procClass(${args.map(expression).mkString(", ")})")
        out.puts(s"$destName = _process.decode($srcName)")
    }
  }

  override def normalIO: String = "self._io"

  override def allocateIO(varName: Identifier, rep: RepeatSpec): String = {
    val varStr = privateMemberName(varName)
    val ioName = s"_io_${idToStr(varName)}"

    val args = rep match {
      case RepeatEos | RepeatUntil(_) => s"$varStr[-1]"
      case RepeatExpr(_) => s"$varStr[i]"
      case NoRepeat => varStr
    }

    out.puts(s"$ioName = $kstreamName(BytesIO($args))")
    ioName
  }

  override def useIO(ioEx: expr): String = {
    out.puts(s"io = ${expression(ioEx)}")
    "io"
  }

  override def pushPos(io: String): Unit =
    out.puts(s"_pos = $io.pos()")

  override def seek(io: String, pos: Ast.expr): Unit =
    out.puts(s"$io.seek(${expression(pos)})")

  override def popPos(io: String): Unit =
    out.puts(s"$io.seek(_pos)")

  override def alignToByte(io: String): Unit =
    out.puts(s"$io.align_to_byte()")

  override def attrDebugStart(attrId: Identifier, attrType: DataType, ios: Option[String], rep: RepeatSpec): Unit = {
    ios.foreach { (io) =>
      val name = attrId match {
        case _: RawIdentifier | _: SpecialIdentifier => return
        case _ => idToStr(attrId)
      }
      rep match {
        case NoRepeat =>
          out.puts(s"self._debug['$name']['start'] = $io.pos()")
        case _: RepeatExpr | RepeatEos | _: RepeatUntil =>
          out.puts(s"if not 'arr' in self._debug['$name']:")
          out.inc
          out.puts(s"self._debug['$name']['arr'] = []")
          out.dec
          out.puts(s"self._debug['$name']['arr'].append({'start': $io.pos()})")
      }
    }
  }

  override def attrPrimitiveWrite(
    io: String,
    exprRaw: String,
    dataType: DataType,
    defEndian: Option[FixedEndian],
    exprTypeOpt: Option[DataType] = None
  ): Unit = {
    val exprType = exprTypeOpt.getOrElse(dataType)
    val expr = exprRaw

    val stmt = dataType match {
      case t: ReadableType =>
        s"$io.write_${t.apiCall(defEndian)}(${expr})"
      case BitsType1 =>
        s"$io.writeBitsInt(1, ($expr) ? 1 : 0)"
      case BitsType(width: Int) =>
        s"$io.writeBitsInt($width, $expr)"
      case _: BytesType =>
        s"$io.write_bytes($expr)"
    }
    out.puts(stmt)
  }

  override def attrBytesLimitWrite(io: String, expr: String, size: String, term: Int, padRight: Int): Unit =
    out.puts(s"$io.write_bytes_limit(self, $expr, $size, $term,$padRight)")

  override def attrUserTypeInstreamWrite(io: String, exprRaw: String, dataType: DataType, exprType: DataType) = {
    val expr = exprRaw
    out.puts(s"$expr._write()")
  }

  override def attrWriteStreamToStream(srcIo: String, dstIo: String) =
    out.puts(s"$dstIo.write_stream($srcIo);")

  override def exprStreamToByteArray(io: String): String =
    s"$io.to_byte_array()"

  override def attrUnprocess(proc: ProcessExpr, varSrc: Identifier, varDest: Identifier): Unit = {
    
    val srcName = privateMemberName(varSrc)
    val destName = privateMemberName(varDest)

    proc match {
      case ProcessXor(xorValue) =>
        out.puts(s"$destName = $kstreamName.process_xor_one($srcName, ${expression(xorValue)});")
 
      case ProcessRotate(isLeft, rotValue) =>
        val expr = if (!isLeft) {
          expression(rotValue)
        } else {
          s"8 - (${expression(rotValue)})"
        }
        out.puts(s"$destName = $kstreamName.process_rotate_left($srcName, $expr, 1);")
      case ProcessCustom(name, args) =>
        val namespace = name.init.mkString(".")
        val procClass = namespace +
          (if (namespace.nonEmpty) "." else "") +
          type2class(name.last)
        val procName = s"_process_${idToStr(varSrc)}"
        out.puts(s"$procClass $procName = new $procClass(${args.map(expression).mkString(", ")});")
        out.puts(s"$destName = $procName.encode($srcName);")
    }


  }

  override def internalEnumIntType(basedOn: IntType): DataType = {
      basedOn match {
      case IntMultiType(signed, _, endian, _) => IntMultiType(signed, Width8, endian, None)
      case _ => IntMultiType(true, Width8, None, None)
    }  
  }

  override def attrDebugEnd(attrId: Identifier, attrType: DataType, io: String, rep: RepeatSpec): Unit = {
    val name = attrId match {
      case _: RawIdentifier | _: SpecialIdentifier => return
      case _ => idToStr(attrId)
    }
    rep match {
      case NoRepeat =>
        out.puts(s"self._debug['$name']['end'] = $io.pos()")
      case _: RepeatExpr =>
        out.puts(s"self._debug['$name']['arr'][i]['end'] = $io.pos()")
      case RepeatEos | _: RepeatUntil =>
        out.puts(s"self._debug['$name']['arr'][len(${privateMemberName(attrId)}) - 1]['end'] = $io.pos()")
    }
  }

  override def condIfHeader(expr: Ast.expr): Unit = {
    out.puts(s"if ${expression(expr)}:")
    out.inc
  }

  override def condRepeatEosHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("i = 0")
    out.puts(s"while not $io.is_eof():")
    out.inc
  }
  override def handleAssignmentRepeatEos(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}.append($expr)")
  override def condRepeatEosFooter: Unit = {
    out.puts("i += 1")
    universalFooter
  }

  override def condRepeatExprHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, repeatExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = [None] * (${expression(repeatExpr)})")
    out.puts(s"${privateMemberName(id)} = [None] * (${expression(repeatExpr)})")
    out.puts(s"for i in range(${expression(repeatExpr)}):")
    out.inc
  }
  override def handleAssignmentRepeatExpr(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}[i] = $expr")

  override def condRepeatUntilHeader(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    if (needRaw)
      out.puts(s"${privateMemberName(RawIdentifier(id))} = []")
    out.puts(s"${privateMemberName(id)} = []")
    out.puts("i = 0")
    out.puts("while True:")
    out.inc
  }

  override def handleAssignmentRepeatUntil(id: Identifier, expr: String, isRaw: Boolean): Unit = {
    val tmpName = translator.doName(if (isRaw) Identifier.ITERATOR2 else Identifier.ITERATOR)
    out.puts(s"$tmpName = $expr")
    out.puts(s"${privateMemberName(id)}.append($tmpName)")
  }

  override def condRepeatUntilFooter(id: Identifier, io: String, dataType: DataType, needRaw: Boolean, untilExpr: expr): Unit = {
    typeProvider._currentIteratorType = Some(dataType)
    out.puts(s"if ${expression(untilExpr)}:")
    out.inc
    out.puts("break")
    out.dec
    out.puts("i += 1")
    out.dec
  }

  override def handleAssignmentSimple(id: Identifier, expr: String): Unit =
    out.puts(s"${privateMemberName(id)}.value = $expr")

  override def handleAssignmentTempVar(dataType: DataType, id: String, expr: String): Unit =
    out.puts(s"$id = $expr")

  override def parseExpr(dataType: DataType, assignType: DataType, io: String, defEndian: Option[FixedEndian]): String = {
    dataType match {
      case t: ReadableType =>
        s"$io.read_${t.apiCall(defEndian)}()"
      case fbt: FixedBytesType =>
        s"$io.read_bytes(${expression(Ast.expr.IntNum(fbt.contents.length))})"
      case blt: BytesLimitType =>
        s"$io.read_bytes(${expression(blt.size)})"
      case _: BytesEosType =>
        s"$io.read_bytes_full()"
      case BytesTerminatedType(terminator, include, consume, eosError, _) =>
        s"$io.read_bytes_term($terminator, ${bool2Py(include)}, ${bool2Py(consume)}, ${bool2Py(eosError)})"
      case BitsType1 =>
        s"$io.read_bits_int(1) != 0"
      case BitsType(width: Int) =>
        s"$io.read_bits_int($width)"
      case t: UserType =>
        val addParams = Utils.join(t.args.map((a) => translator.translate(a)), "", ", ", ", ")
        val addArgs = if (t.isOpaque) {
          ""
        } else {
          val parent = t.forcedParent match {
            case Some(fp) => translator.translate(fp)
            case None => "self"
          }
          val addEndian = t.classSpec.get.meta.endian match {
            case Some(InheritedEndian) => ", self._is_le"
            case _ => ""
          }
          s", $parent, self._root$addEndian"
        }
        s"${userType2class(t)}($addParams$io$addArgs)"
    }
  }

  override def bytesPadTermExpr(expr0: String, padRight: Option[Int], terminator: Option[Int], include: Boolean) = {
    val expr1 = padRight match {
      case Some(padByte) => s"$kstreamName.bytes_strip_right($expr0, $padByte)"
      case None => expr0
    }
    val expr2 = terminator match {
      case Some(term) => s"$kstreamName.bytes_terminate($expr1, $term, ${bool2Py(include)})"
      case None => expr1
    }
    expr2
  }

  override def userTypeDebugRead(id: String): Unit =
    out.puts(s"$id._read()")

  override def switchStart(id: Identifier, on: Ast.expr): Unit = {
    out.puts(s"_on = ${expression(on)}")
  }

  override def switchCaseFirstStart(condition: Ast.expr): Unit = {
    out.puts(s"if _on == ${expression(condition)}:")
    out.inc
  }

  override def switchCaseStart(condition: Ast.expr): Unit = {
    out.puts(s"elif _on == ${expression(condition)}:")
    out.inc
  }

  override def switchCaseEnd(): Unit =
    out.dec

  override def switchElseStart(): Unit = {
    out.puts(s"else:")
    out.inc
  }

  override def switchEnd(): Unit = {}

  override def instanceHeader(className: String, instName: InstanceIdentifier, dataType: DataType, isNullable: Boolean): Unit = {
    out.puts("@property")
    out.puts(s"def ${publicMemberName(instName)}(self):")
    out.inc
  }

  override def instanceCheckCacheAndReturn(instName: InstanceIdentifier, dataType: DataType): Unit = {
    out.puts(s"if hasattr(self, '${idToStr(instName)}'):")
    out.inc
    instanceReturn(instName, dataType)
    out.dec
    out.puts
  }

  override def instanceReturn(instName: InstanceIdentifier, attrType: DataType): Unit = {
    // not very efficient, probably should be some other way to do that, but for now it will do:
    // workaround to avoid Python generating an "AttributeError: instance has no attribute"
    out.puts(s"return ${privateMemberName(instName)} if hasattr(self, '${idToStr(instName)}') else None")
  }

  override def enumDeclaration(curClass: String, enumName: String, enumColl: Seq[(Long, String)]): Unit = {
    importList.add("from enum import Enum")

    out.puts
    out.puts(s"class ${type2class(enumName)}(Enum):")
    out.inc
    enumColl.foreach { case (id: Long, label: String) => out.puts(s"$label = $id") }
    out.dec
  }

  override def debugClassSequence(seq: List[AttrSpec]) = {
    val seqStr = seq.map((attr) => "\"" + idToStr(attr.id) + "\"").mkString(", ")
    out.puts(s"SEQ_FIELDS = [$seqStr]")
  }

  def bool2Py(b: Boolean): String = if (b) { "True" } else { "False" }

  def idToStr(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case NumberedIdentifier(idx) => s"_${NumberedIdentifier.TEMPLATE}$idx"
      case InstanceIdentifier(name) => s"_m_$name"
      case RawIdentifier(innerId) => s"_raw_${idToStr(innerId)}"
    }
  }

  override def privateMemberName(id: Identifier): String = s"self.${idToStr(id)}"

  override def publicMemberName(id: Identifier): String = {
    id match {
      case SpecialIdentifier(name) => name
      case NamedIdentifier(name) => name
      case InstanceIdentifier(name) => name
      case RawIdentifier(innerId) => s"_raw_${publicMemberName(innerId)}"
    }
  }

  override def localTemporaryName(id: Identifier): String = s"_t_${idToStr(id)}"

  override def ksErrorName(err: KSError): String = PythonCompiler.ksErrorName(err)

  override def attrValidateExpr(
    attrId: Identifier,
    attrType: DataType,
    checkExpr: Ast.expr,
    errName: String,
    errArgs: List[Ast.expr]
  ): Unit = {
    val errArgsStr = errArgs.map(translator.translate).mkString(", ")
    out.puts(s"if not ${translator.translate(checkExpr)}:")
    out.inc
    out.puts(s"raise $errName($errArgsStr)")
    out.dec
  }

  def userType2class(t: UserType): String = {
    val name = t.classSpec.get.name
    val firstName = name.head
    val prefix = if (t.isOpaque && firstName != translator.provider.nowClass.name.head) {
      s"$firstName."
    } else {
      ""
    }
    s"$prefix${types2class(name)}"
  }


  override def getTypeDataType(datatype: DataType): String = {
    val native_type = datatype match {
      case int_type: IntType => "int"
      case str_type: FloatMultiType => "float"
      case b_type: BytesType => "bytes"
      case t: StrFromBytesType => "str"
      case u_type: UserType => {
        userType2class(u_type)
      }
      case enum: EnumType => {
        types2class(enum.enumSpec.get.name)
      }
      case _ => ""
    } 
    native_type
  }

  override def defineArrayType(id: Identifier, datatype: DataType): Unit = {
    out.puts(s"${privateMemberName(id)} = ArrayKaitaiField()")

  }

  def generateIntFieldObject(inttype: IntType, switchValOn: String, switchValCases: String): String = {
    inttype match {
      case int1type: Int1Type =>
        s"IntKaitaiField(${translator.doBoolLiteral(int1type.signed)}, ${int1type.maxValue.getOrElse("None")}, ${int1type.minValue.getOrElse("None")}, 8, switch_value_on= $switchValOn , switch_value = $switchValCases)"
      case intmultitype: IntMultiType => 
        s"IntKaitaiField(${translator.doBoolLiteral(intmultitype.signed)}, ${intmultitype.maxValue.getOrElse("None")}, ${intmultitype.minValue.getOrElse("None")}, ${intmultitype.width.width*8}, switch_value_on= $switchValOn , switch_value = $switchValCases)"
    }
   }

  def generateFloatFieldObject(floattype: FloatMultiType, switchValOn: String, switchValCases: String): String = {
     s"FloatKaitaiField(${floattype.maxValue.getOrElse("None")}, switch_value_on= $switchValOn , switch_value = $switchValCases)"
   }

  def generateStringFieldObject(strtype: StrType, switchValOn: String, switchValCases: String): String = {
     val choiceString: StringBuilder = StringBuilder.newBuilder
     strtype.choices match {
       case Some(choice) => {
         choiceString.append("[")
         for (c <- choice) choiceString.append(s"${'"'}${c}${'"'},")
         choiceString.append("]")
        }
       case None => choiceString.append("None")
      }
      s"StringKaitaiField(None, ${choiceString.result()}, switch_value_on= $switchValOn , switch_value = $switchValCases)\n"
   }
  
  def generateBytesFieldObject(bytestype: BytesType, switchValOn: String, switchValCases: String): String = {
     val choiceString: StringBuilder = StringBuilder.newBuilder
     bytestype.choices match {
       case Some(choice) => {
         choiceString.append("[")
         for (c <- choice) choiceString.append(s"${'"'}${c}${'"'},")
         choiceString.append("]")
        }
       case None => choiceString.append("None")
      }
      s"BytesKaitaiField(None, ${choiceString.result()}, switch_value_on= $switchValOn , switch_value = $switchValCases)\n"
   }

  override def itrFields(): Unit = {
    out.puts("def __iter__(self):")
    out.inc
    out.puts("prev_data = bytes()")
    out.puts("for f in self._fields:")
    out.inc
    out.puts("if f.interaction == PacketType.transmit:")
    out.inc
    out.puts("yield TransmitInteraction(f._value.generate())")
    out.dec
    out.puts("else:")
    out.inc
    out.puts("tmp_recv = ReceiveInteraction()")
    out.puts("yield tmp_recv")
    out.puts("prev_data += tmp_recv.data")
    out.puts("f._read(BytesIO(prev_data))")
    out.puts("prev_data = prev_data[len(f.data):len(prev_data)]")
    out.dec
    out.dec
    out.dec
    out.puts
  }

  override def defineReadEnd(id: Identifier, constraints: Map[String,expr]): Unit = {

    var mapString: StringBuilder = new StringBuilder
    mapString.append("{ ")
    for ((k, v) <- constraints ) {
      mapString.append(s"'$k': lambda: ${expression(v)}, ")
    }
    mapString.append("}")
    println(s"Here: ${mapString.result()}")

    out.puts(s"self._constraints[${privateMemberName(id)}] = ${mapString.result()}")

  }

  override def defineExports(id: Identifier, constraints: Map[String,Ast.expr]): Unit = {


    for ((k, v) <- constraints ) {
      out.puts(s"exports['$k'] = lambda: ${expression(v)}")
    }

  }

  override def defineReadStart(id: Identifier, datatype: DataType, switchOnValue: Option[SwitchValueSpec], interaction: InteractionSpec): Unit = {
    val nativeType = getTypeDataType(datatype)

    val switchValOn = defineSwitchOn(switchOnValue)
    val switchValCases = defineSwitchMap(switchOnValue)

    val interactionStr: String = interaction match {
      case TransmitInteraction => "interaction = PacketType.transmit"
      case ReceiveInteraction => "interaction = PacketType.receive"
      case DelayInteraction => "interaction = PacketType.delay"
      case NonInteraction => ""
    }

    datatype match {
      
      case inttype : IntType=>
        out.puts(
          s"${privateMemberName(id)} = ${generateIntFieldObject(inttype, switchValOn, switchValCases)}"
          )
      case floattype : FloatMultiType=> 
        out.puts(s"${privateMemberName(id)} = ${generateFloatFieldObject(floattype, switchValOn, switchValCases)}")
      case strtype : StrType => {
        out.puts(s"${privateMemberName(id)} = ${generateStringFieldObject(strtype, switchValOn, switchValCases)}\n")
      }
      case bytestype : BytesType => {
        out.puts(s"${privateMemberName(id)} = ${generateBytesFieldObject(bytestype, switchValOn, switchValCases)}\n")
      }
      case switchtype: SwitchType => {
        var mapString: StringBuilder = StringBuilder.newBuilder
        mapString.append("{ ")
        for ((k, v) <- switchtype.cases) {
          var s = v match {
            case inttype: IntType => generateIntFieldObject(inttype, switchValOn, switchValCases)
            case floattype: FloatMultiType => generateFloatFieldObject(floattype, switchValOn, switchValCases)
            case strtype: StrType => generateStringFieldObject(strtype, switchValOn, switchValCases)
            case bytestype: BytesType => generateBytesFieldObject(bytestype, switchValOn, switchValCases)
          }
          mapString.append(s"${expression(k)}: ${s},")
        }
        mapString.append(" }")
        
        out.puts(s"${privateMemberName(id)} = SwitchTypeKaitaiField(${expression(switchtype.on)}, ${mapString.result()})")

      }
      case _ => {
        out.puts(s"${privateMemberName(id)} = KaitaiField($nativeType, $interactionStr)")
        out.puts(s"${privateMemberName(id)}._value = $nativeType(self._io,self, self._root)")
      }
    }
    out.puts(s"self._fields.append(${privateMemberName(id)})")
    
    
  }

  override def defineSwitchMap(switchValSpec: Option[SwitchValueSpec]): String = {
    
    switchValSpec match {
      case None =>
        "None"
      case Some(x) =>
        x match {
          case switchvalspec: SwitchValueSpec => {
            var s = StringBuilder.newBuilder
            s.append("{")
            for ((k, v) <- switchvalspec.cases) { s.append(s"${expression(k)}: $v,") }
            s.append("}")
            // println(s.result())
            s.result()
          }
        }
    }

  }

  override def defineSwitchOn(switchvalSpec: Option[SwitchValueSpec]): String = {
    switchvalSpec match {
      case None =>
        "None"
      case Some(x) =>
        x match {
          case switchvalspec: SwitchValueSpec => {
            expression(x.on)
          }
          case _=>
            "None"
        }
    }
  }

}

object PythonCompiler extends LanguageCompilerStatic
  with UpperCamelCaseClasses
  with StreamStructNames
  with ExceptionNames {
  override def getCompiler(
    tp: ClassTypeProvider,
    config: RuntimeConfig
  ): LanguageCompiler = new PythonCompiler(tp, config)

  override def kstreamName: String = "KaitaiStream"
  override def kstructName: String = "KaitaiStruct"
  def kfieldName: String = "KaitaiField"
  override def ksErrorName(err: KSError): String = err match {
    case EndOfStreamError => "EOFError"
    case _ => s"kaitaistruct.${err.name}"
  }

  def types2class(name: List[String]): String = {
    if (name.size > 1) {
      val path = name.drop(1).map(x => type2class(x)).mkString(".")
      s"self._root.$path"
    } else {
      type2class(name.head)
    }
  }
}
