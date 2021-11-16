package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

import scala.collection.mutable

/**
  * Type that we use when we want to refer to a class specification or something
  * close, but not (yet) that well defined.
  */

sealed trait ClassLikeSpec {
  def path: List[String]
  def meta: MetaSpec
  def doc: DocSpec
  def types: Map[String, StructSpec]
  def variables: Map[NamedIdentifier, VariableSpec]
}

abstract class ClassSpec extends ClassLikeSpec {
  var parentClass: ClassSpec = UnknownSpecClass$
  def parentType: DataType = parentClass match {
    case UnknownSpecClass$ | GenericSpecStructClass$ => CalcKaitaiStructType
    case t: StructSpec => CalcUserType(t.name, None)
  }

  /**
    * The class specification that this class is nested into, if it exists.
    * For top-level classes, it's None.
    */
  var upClass: Option[ClassSpec] = None

  /**
    * Full absolute name of the class (including all names of classes that
    * it's nested into, as a namespace). Derived either from `meta`/`id`
    * (for top-level classes), or from keys in `types` (for nested classes).
    */
  var name = List[String]()

  /**
    * @return Absolute name of class as string, components separated by
    *         double colon operator `::`
    */
  def nameAsStr = name.mkString("::")

  def forEach(proc: (ClassSpec) => Unit): Unit = {
    types.foreach { case (_, typeSpec) =>
      proc(typeSpec)
    }
  }

  /**
    * Recursively traverses tree of subtypes calling certain function for every type.
    */
  def forEachRec(proc: (ClassSpec) => Unit): Unit = {
    proc.apply(this)
    forEach(_.forEachRec(proc))
  }

  def markupClassNames: Unit = {
    types.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = name ::: List(nestedName)
      nestedClass.upClass = Some(this)
      nestedClass.markupClassNames
    }
  }
}

abstract class ClassWithSeqSpec extends ClassSpec {
  def seq: List[AttrLikeSpec]

  var seqSize: Sized = NotCalculatedSized
}

case object UnknownSpecClass$ extends ClassSpec {
  override def path: List[String] = List()
  override def meta: MetaSpec = MetaSpec.OPAQUE
  override def doc: DocSpec = DocSpec.EMPTY
  override def types: Map[String, StructSpec] = Map()
  override def variables: Map[NamedIdentifier, VariableSpec] = Map()
}
case object GenericSpecStructClass$ extends ClassSpec {
  override def path: List[String] = List()
  override def meta: MetaSpec = MetaSpec.OPAQUE
  override def doc: DocSpec = DocSpec.EMPTY
  override def types: Map[String, StructSpec] = Map()
  override def variables: Map[NamedIdentifier, VariableSpec] = Map()
}

sealed trait Sized
case object DynamicSized extends Sized
case object NotCalculatedSized extends Sized
case object StartedCalculationSized extends Sized
case class FixedSized(n: Int) extends Sized

case class StructSpec(path: List[String], meta: MetaSpec, doc: DocSpec, types: Map[String, StructSpec], params: List[ParamDefSpec], seq: List[AttrLikeSpec], isTopLevel: Boolean, instances: Map[InstanceIdentifier, InstanceSpec], enums: Map[String, EnumSpec], variables: Map[NamedIdentifier, VariableSpec])
extends ClassWithSeqSpec with YAMLPath {
  override def equals(obj: Any): Boolean = obj match {
    case other: StructSpec =>
      path == other.path &&
      isTopLevel == other.isTopLevel &&
      meta == other.meta &&
      doc == other.doc &&
      params == other.params &&
      seq == other.seq &&
      types == other.types &&
      instances == other.instances &&
      enums == other.enums &&
      name == other.name
    case _ => false
  }

  /**
    * Recursively traverses tree of types starting from this type, calling
    * certain function for every type, starting from this one.
    */
  override def forEachRec(proc: ClassSpec => Unit): Unit = {
    proc.apply(this)
    forEach(_.forEachRec(proc))
  }

  override def markupClassNames: Unit = {
    super.markupClassNames
    enums.foreach { case (enumName, enumSpec) =>
      enumSpec.name = name ::: List(enumName)
    }
  }
}

object StructSpec {
  val LEGAL_KEYS = Set(
    "meta",
    "doc",
    "doc-ref",
    "params",
    "seq",
    "types",
    "instances",
    "enums",
    "variables"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec): StructSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val metaPath = path ++ List("meta")
    val explicitMeta = srcMap.get("meta").map(MetaSpec.fromYaml(_, metaPath)).getOrElse(MetaSpec.emptyWithPath(metaPath))
    val meta = explicitMeta.fillInDefaults(metaDef)

    val doc = DocSpec.fromYaml(srcMap, path)

    val params: List[ParamDefSpec] = srcMap.get("params") match {
      case Some(value) => paramDefFromYaml(value, path ++ List("params"))
      case None => List()
    }
    val seq: List[AttrSpec] = srcMap.get("seq") match {
      case Some(value) => seqFromYaml(value, path ++ List("seq"), meta)
      case None => List()
    }
    val types: Map[String, StructSpec] = srcMap.get("types") match {
      case Some(value) => typesFromYaml(value, path ++ List("types"), meta)
      case None => Map()
    }
    val instances: Map[InstanceIdentifier, InstanceSpec] = srcMap.get("instances") match {
      case Some(value) => instancesFromYaml(value, path ++ List("instances"), meta)
      case None => Map()
    }
    val enums: Map[String, EnumSpec] = srcMap.get("enums") match {
      case Some(value) => enumsFromYaml(value, path ++ List("enums"))
      case None => Map()
    }
    val variables: Map[NamedIdentifier, VariableSpec] = srcMap.get("variables") match {
      case Some(value) => variablesFromYaml(value, path ++ List("variables"), meta)
      case None => Map()
    }

    checkDupSeqInstIds(seq, instances)

    val cs = StructSpec(path, meta, doc, types, params, seq, path.isEmpty, instances, enums, variables)

    // If that's a top-level class, set its name from meta/id
    if (path.isEmpty) {
      explicitMeta.id match {
        case None =>
          throw new YAMLParseException("no `meta/id` encountered in top-level class spec", path ++ List("meta", "id"))
        case Some(id) =>
          cs.name = List(id)
      }
    }

    cs
  }

  def paramDefFromYaml(src: Any, path: List[String]): List[ParamDefSpec] = {
    src match {
      case srcList: List[Any] =>
        val params = srcList.zipWithIndex.map { case (attrSrc, idx) =>
          ParamDefSpec.fromYaml(attrSrc, path ++ List(idx.toString), idx)
        }
        // FIXME: checkDupSeqIds(params)
        params
      case unknown =>
        throw new YAMLParseException(s"expected array, found $unknown", path)
    }
  }

  def seqFromYaml(src: Any, path: List[String], metaDef: MetaSpec): List[AttrSpec] = {
    src match {
      case srcList: List[Any] =>
        val seq = srcList.zipWithIndex.map { case (attrSrc, idx) =>
          AttrSpec.fromYaml(attrSrc, path ++ List(idx.toString), metaDef, idx)
        }
        checkDupSeqIds(seq)
        seq
      case unknown =>
        throw new YAMLParseException(s"expected array, found $unknown", path)
    }
  }

  def checkDupSeqIds(seq: List[AttrSpec]): Unit = {
    val attrIds = mutable.Map[String, AttrSpec]()
    seq.foreach { (attr) =>
      attr.id match {
        case NamedIdentifier(id) =>
          checkDupId(attrIds.get(id), id, attr)
          attrIds.put(id, attr)
        case _ => // do nothing with non-named IDs
      }
    }
  }

  def checkDupSeqInstIds(seq: List[AttrSpec], instances: Map[InstanceIdentifier, InstanceSpec]): Unit = {
    val attrIds: Map[String, AttrSpec] = seq.flatMap((attr) => attr.id match {
      case NamedIdentifier(id) => Some(id -> attr)
      case _ => None
    }).toMap

    instances.foreach { case (id, instSpec) =>
      checkDupId(attrIds.get(id.name), id.name, instSpec)
    }
  }

  private def checkDupId(prevAttrOpt: Option[AttrSpec], id: String, nowAttr: YAMLPath) {
    prevAttrOpt match {
      case Some(prevAttr) =>
        throw new YAMLParseException(
          s"duplicate attribute ID '$id', previously defined at /${prevAttr.pathStr}",
          nowAttr.path
        )
      case None =>
        // no dups, ok
    }
  }

  def typesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, StructSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (typeName, body) =>
      Identifier.checkIdentifierSource(typeName, "type", path ++ List(typeName))
      typeName -> StructSpec.fromYaml(body, path ++ List(typeName), metaDef)
    }
  }

  def instancesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[InstanceIdentifier, InstanceSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val instName = ParseUtils.asStr(key, path)
      Identifier.checkIdentifierSource(instName, "instance", path ++ List(instName))
      val id = InstanceIdentifier(instName)
      id -> InstanceSpec.fromYaml(body, path ++ List(instName), metaDef, id)
    }
  }

  def enumsFromYaml(src: Any, path: List[String]): Map[String, EnumSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val enumName = ParseUtils.asStr(key, path)
      Identifier.checkIdentifierSource(enumName, "enum", path ++ List(enumName))
      enumName -> EnumSpec.fromYaml(body, path ++ List(enumName))
    }
  }

  def variablesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[NamedIdentifier, VariableSpec] = {
    val srcMap = ParseUtils.asMap(src, path)
    srcMap.map { case (key, body) =>
      val varName = ParseUtils.asStr(key, path)
      Identifier.checkIdentifierSource(varName, "variable", path ++ List(varName))
      val id = NamedIdentifier(varName)
      id -> VariableSpec.fromYaml(body, path ++ List(varName), metaDef, id)
    }
  }

  def fromYaml(src: Any): StructSpec = fromYaml(src, List(), MetaSpec.OPAQUE)

  def opaquePlaceholder(typeName: List[String]): StructSpec = {
    val placeholder = StructSpec(List(), meta = MetaSpec.OPAQUE, doc = DocSpec.EMPTY, types = Map(), params = List(), seq = List(), true, instances = Map(), enums = Map(), Map())
    placeholder.name = typeName
    placeholder
  }
}
