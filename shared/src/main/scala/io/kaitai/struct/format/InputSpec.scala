package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

import scala.collection.mutable

case class InputSpec(
  path: List[String],
  meta: MetaSpec,
  doc: DocSpec,
  types: Map[String, StructSpec],
  seq: List[InteractionSpec],
  variables: Map[NamedIdentifier, VariableSpec]
) extends ClassWithSeqSpec with YAMLPath {
  override def forEachRec(proc: ClassSpec => Unit): Unit = {
    proc.apply(this)
    forEach(_.forEachRec(proc))
  }
}

object InputSpec {
  val LEGAL_KEYS = Set(
    "meta",
    "doc",
    "doc-ref",
    "seq",
    "types",
    "variables"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec): InputSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val metaPath = path ++ List("meta")
    val explicitMeta = srcMap.get("meta").map(MetaSpec.fromYaml(_, metaPath)).getOrElse(MetaSpec.emptyWithPath(metaPath))
    val meta = explicitMeta.fillInDefaults(metaDef)

    val doc = DocSpec.fromYaml(srcMap, path)

    val seq: List[InteractionSpec] = srcMap.get("seq") match {
      case Some(value) => seqFromYaml(value, path ++ List("seq"), meta)
      case None => List()
    }
    val types: Map[String, StructSpec] = srcMap.get("types") match {
      case Some(value) => typesFromYaml(value, path ++ List("types"), meta)
      case None => Map()
    }
    val variables: Map[NamedIdentifier, VariableSpec] = srcMap.get("variables") match {
      case Some(value) => variablesFromYaml(value, path ++ List("variables"), meta)
      case None => Map()
    }

    val cs = InputSpec(
      path,
      meta, doc,
      types, seq,
      variables
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

  def seqFromYaml(src: Any, path: List[String], metaDef: MetaSpec): List[InteractionSpec] = {
    src match {
      case srcList: List[Any] =>
        val seq = srcList.zipWithIndex.map { case (attrSrc, idx) =>
          InteractionSpec.fromYaml(attrSrc, path ++ List(idx.toString), metaDef, idx)
        }
        seq
      case unknown =>
        throw new YAMLParseException(s"expected array, found $unknown", path)
    }
  }

  def typesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, StructSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (typeName, body) =>
      Identifier.checkIdentifierSource(typeName, "type", path ++ List(typeName))
      typeName -> StructSpec.fromYaml(body, path ++ List(typeName), metaDef)
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

  def fromYaml(src: Any): InputSpec = fromYaml(src, List(), MetaSpec.OPAQUE)
}
