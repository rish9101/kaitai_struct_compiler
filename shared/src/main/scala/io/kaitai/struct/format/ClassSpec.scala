package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

import scala.collection.mutable

/**
  * Type that we use when we want to refer to a class specification or something
  * close, but not (yet) that well defined.
  */
sealed trait ClassSpecLike
case object UnknownClassSpec extends ClassSpecLike
case object GenericStructClassSpec extends ClassSpecLike

sealed trait Sized
case object DynamicSized extends Sized
case object NotCalculatedSized extends Sized
case object StartedCalculationSized extends Sized
case class FixedSized(n: Int) extends Sized

case class ClassSpec(
  path: List[String],
  isTopLevel: Boolean,
  meta: MetaSpec,
  doc: DocSpec,
  params: List[ParamDefSpec],
  seq: List[AttrSpec],
  types: Map[String, ClassSpec],
  instances: Map[InstanceIdentifier, InstanceSpec],
  enums: Map[String, EnumSpec],
  inputs: Map[String, ClassSpec],
  interactions: Map[String, ClassSpec]
) extends ClassSpecLike with YAMLPath {
  var parentClass: ClassSpecLike = UnknownClassSpec

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

  /**
    * The class specification that this class is nested into, if it exists.
    * For top-level classes, it's None.
    */
  var upClass: Option[ClassSpec] = None

  var seqSize: Sized = NotCalculatedSized

  def parentType: DataType = parentClass match {
    case UnknownClassSpec | GenericStructClassSpec => CalcKaitaiStructType
    case t: ClassSpec => CalcUserType(t.name, None)
  }

  /**
    * Recursively traverses tree of types starting from this type, calling
    * certain function for every type, starting from this one.
    */
  def forEachRec(proc: (ClassSpec) => Unit): Unit = {
    proc.apply(this)
    types.foreach { case (_, typeSpec) =>
      typeSpec.forEachRec(proc)
    }
  }

  override def equals(obj: Any): Boolean = obj match {
    case other: ClassSpec =>
      path == other.path &&
      isTopLevel == other.isTopLevel &&
      meta == other.meta &&
      doc == other.doc &&
      params == other.params &&
      seq == other.seq &&
      types == other.types &&
      instances == other.instances &&
      enums == other.enums &&
      inputs == other.inputs &&
      interactions == other.interactions &&
      name == other.name
    case _ => false
  }
}

object ClassSpec {
  val LEGAL_KEYS = Set(
    "meta",
    "doc",
    "doc-ref",
    "params",
    "seq",
    "types",
    "instances",
    "enums",
    "inputs",
    "interactions"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec): ClassSpec = {
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
    val types: Map[String, ClassSpec] = srcMap.get("types") match {
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
    val inputs: Map[String, ClassSpec] = srcMap.get("inputs") match {
      case Some(value) => inputsFromYaml(value, path ++ List("inputs"), meta)
      case None => Map()
    }
    val interactions: Map[String, ClassSpec] = srcMap.get("interactions") match {
      case Some(value) => interactionsFromYaml(value, path ++ List("interactions"), meta)
      case None => Map()
    }

    checkDupSeqInstIds(seq, instances)

    val cs = ClassSpec(
      path, path.isEmpty,
      meta, doc,
      params, seq, types, instances, enums, inputs, interactions
    )

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

  def typesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, ClassSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (typeName, body) =>
      Identifier.checkIdentifierSource(typeName, "type", path ++ List(typeName))
      typeName -> ClassSpec.fromYaml(body, path ++ List(typeName), metaDef)
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

  def inputsFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, ClassSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (inputName, body) =>
      Identifier.checkIdentifierSource(inputName, "inputs", path ++ List(inputName))
      inputName -> ClassSpec.fromYaml(body, path ++ List(inputName), metaDef)
    }
  }

  def interactionsFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, ClassSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (interactionName, body) =>
      Identifier.checkIdentifierSource(interactionName, "interactions", path ++ List(interactionName))
      interactionName -> ClassSpec.fromYaml(body, path ++ List(interactionName), metaDef)
    }
  }

  def fromYaml(src: Any): ClassSpec = fromYaml(src, List(), MetaSpec.OPAQUE)

  def opaquePlaceholder(typeName: List[String]): ClassSpec = {
    val placeholder = ClassSpec(
      List(),
      true,
      meta = MetaSpec.OPAQUE,
      doc = DocSpec.EMPTY,
      params = List(),
      seq = List(),
      types = Map(),
      instances = Map(),
      enums = Map(),
      inputs = Map(),
      interactions = Map()
    )
    placeholder.name = typeName
    placeholder
  }
}
