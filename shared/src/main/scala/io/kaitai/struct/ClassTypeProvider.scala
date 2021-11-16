package io.kaitai.struct

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.format._
import io.kaitai.struct.precompile.{EnumNotFoundError, FieldNotFoundError, TypeNotFoundError, TypeUndecidedError}
import io.kaitai.struct.translators.TypeProvider

class ClassTypeProvider(classSpecs: ProtocolSpecs, var topClass: ClassSpec) extends TypeProvider {
  var nowClass = topClass

  var _currentIteratorType: Option[DataType] = None
  var _currentSwitchType: Option[DataType] = None
  def currentIteratorType: DataType = _currentIteratorType.get
  def currentSwitchType: DataType = _currentSwitchType.get

  override def determineType(attrName: String): DataType = {
    determineType(nowClass, attrName)
  }

  override def determineType(inClass: ClassSpec, attrName: String): DataType = {
    attrName match {
      case Identifier.GLOBAL => GlobalType()
      case Identifier.THIS => makeUserType(inClass)
      case Identifier.ROOT =>
        makeUserType(topClass)
      case Identifier.PARENT => inClass match {
        case inClass: StructSpec =>
          if (inClass.parentClass == UnknownSpecClass$)
            throw new RuntimeException(s"Unable to derive ${Identifier.PARENT} type in ${inClass.nameAsStr}")
          makeUserType(inClass.parentClass)
        case _ =>
          throw new RuntimeException(s"Unable to derive ${Identifier.PARENT} type in ${inClass.nameAsStr}")
      }
      case Identifier.IO =>
        KaitaiStreamType
      case Identifier.ITERATOR =>
        currentIteratorType
      case Identifier.SWITCH_ON =>
        currentSwitchType
      case Identifier.INDEX =>
        CalcIntType
      case Identifier.SIZEOF =>
        CalcIntType
      case Identifier.EXCLUDES =>
        ArrayType(CalcStrType)
      case _ => {
        inClass.variables.get(NamedIdentifier(attrName)) match {
          case Some(v: VariableSpec) =>
            val dt = v.dataType match {
              case Some(t) => return t
              case None => throw new TypeUndecidedError(attrName)
            }
          case None =>
        }
        inClass match {
          case inClass: ClassWithSeqSpec =>
            inClass.seq.foreach { el =>
              if (el.id == NamedIdentifier(attrName))
                return el.dataTypeComposite
            }
          case _ =>
        }
        inClass match {
          case inClass: StructSpec =>
            inClass.params.foreach { el =>
              if (el.id == NamedIdentifier(attrName))
                return el.dataType
            }
            inClass.instances.get(InstanceIdentifier(attrName)) match {
              case Some(i: ValueInstanceSpec) =>
                val dt = i.dataType match {
                  case Some(t) => t
                  case None => throw new TypeUndecidedError(attrName)
                }
                return dt
              case Some(i: ParseInstanceSpec) => return i.dataTypeComposite
              case None => // do nothing
            }
          case _ => // do nothing
        }
        throw new FieldNotFoundError(attrName, inClass)
      }
    }
  }

  def makeUserType(csl: ClassSpec): DataType = {
    csl match {
      case GenericSpecStructClass$ =>
        KaitaiStructType
      case cs: ClassSpec =>
        val ut = CalcUserType(cs.name, None)
        ut.classSpec = Some(cs)
        ut
    }
  }

  override def resolveEnum(inType: Ast.typeId, enumName: String): EnumSpec =
    resolveEnum(resolveClassSpec(inType), enumName)

  def resolveEnum(inClass: StructSpec, enumName: String): EnumSpec = {
    inClass.enums.get(enumName) match {
      case Some(spec) =>
        spec
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass: StructSpec) => resolveEnum(upClass, enumName)
          case None =>
            throw new EnumNotFoundError(enumName, nowClass)
        }
    }
  }

  override def resolveType(typeName: Ast.typeId): DataType =
    makeUserType(resolveClassSpec(typeName))

  def resolveClassSpec(typeName: Ast.typeId): StructSpec =
    resolveClassSpec(
      if (typeName.absolute) topClass else nowClass,
      typeName.names
    )

  def resolveClassSpec(inClass: ClassSpec, typeName: Seq[String]): StructSpec = {
    if (typeName.isEmpty)
      inClass match {
        case inClass: StructSpec =>
          return inClass
        case _ =>
          throw new TypeNotFoundError(typeName.mkString("::"), nowClass)
      }

    val headTypeName :: restTypesNames = typeName.toList
    val nextClass = resolveClassSpec(inClass, headTypeName)
    if (restTypesNames.isEmpty) {
      nextClass
    } else {
      resolveClassSpec(nextClass, restTypesNames)
    }
  }

  def resolveClassSpec(inClass: ClassSpec, typeName: String): StructSpec = {
    if (inClass.name.last == typeName)
      inClass match {
        case inClass: StructSpec =>
          return inClass
        case _ =>
          throw new TypeNotFoundError(typeName, nowClass)
      }

    inClass.types.get(typeName) match {
      case Some(spec) =>
        spec
      case None =>
        // let's try upper levels of hierarchy
        inClass.upClass match {
          case Some(upClass) => resolveClassSpec(upClass, typeName)
          case None =>
            throw new TypeNotFoundError(typeName, nowClass)
        }
    }
  }

  override def isLazy(attrName: String): Boolean = nowClass match {
    case nowClass: StructSpec => isLazy(nowClass, attrName)
    case _ => throw new FieldNotFoundError(attrName, nowClass)
  }

  def isLazy(inClass: StructSpec, attrName: String): Boolean = {
    inClass.seq.foreach { el =>
      if (el.id == NamedIdentifier(attrName))
        return false
    }
    inClass.params.foreach { el =>
      if (el.id == NamedIdentifier(attrName))
        return false
    }
    inClass.instances.get(InstanceIdentifier(attrName)) match {
      case Some(i) =>
        return true
      case None =>
        // do nothing
    }
    throw new FieldNotFoundError(attrName, inClass)
  }
}
