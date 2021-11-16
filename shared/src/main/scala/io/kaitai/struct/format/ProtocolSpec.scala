package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._

import scala.collection.mutable

case class ProtocolSpec(
  path: List[String],
  meta: MetaSpec,
  doc: DocSpec,
  types: Map[String, StructSpec],
  inputs: Map[String, InputSpec],
  stateMachine: StateMachineSpec,
  variables: Map[NamedIdentifier, VariableSpec]
) extends ClassSpec with YAMLPath {

  override def forEach(proc: (ClassSpec) => Unit): Unit = {
    super.forEach(proc)
    inputs.foreach { case (_, typeSpec) =>
      proc(typeSpec)
    }
  }

  override def markupClassNames: Unit = {
    super.markupClassNames
    inputs.foreach { case (nestedName: String, nestedClass) =>
      nestedClass.name = name ::: List(nestedName)
      nestedClass.upClass = Some(this)
      nestedClass.markupClassNames
    }
  }
}

object ProtocolSpec {
  val LEGAL_KEYS = Set(
    "meta",
    "doc",
    "doc-ref",
    "states",
    "types",
    "inputs",
    "variables"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec): ProtocolSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    val metaPath = path ++ List("meta")
    val explicitMeta = srcMap.get("meta").map(MetaSpec.fromYaml(_, metaPath)).getOrElse(MetaSpec.emptyWithPath(metaPath))
    val meta = explicitMeta.fillInDefaults(metaDef)

    val doc = DocSpec.fromYaml(srcMap, path)

    val types: Map[String, StructSpec] = srcMap.get("types") match {
      case Some(value) => typesFromYaml(value, path ++ List("types"), meta)
      case None => Map()
    }
    val inputs: Map[String, InputSpec] = srcMap.get("inputs") match {
      case Some(value) => inputsFromYaml(value, path ++ List("inputs"), meta)
      case None => Map()
    }
    val stateMachine: StateMachineSpec = srcMap.get("states") match {
      case Some(value) =>
        val stateMap = ParseUtils.asMapStr(value, path ++ List("states"))
        StateMachineSpec.fromYaml(stateMap, path ++ List("states"))
      case None => StateMachineSpec.EMPTY
    }
    val variables: Map[NamedIdentifier, VariableSpec] = srcMap.get("variables") match {
      case Some(value) => variablesFromYaml(value, path ++ List("variables"), meta)
      case None => Map()
    }

    val ps = ProtocolSpec(
      path,
      meta, doc,
      types, inputs, stateMachine,
      variables
    )

    // If that's a top-level class, set its name from meta/id
    if (path.isEmpty) {
      explicitMeta.id match {
        case None =>
          throw new YAMLParseException("no `meta/id` encountered in top-level protocol spec", path ++ List("meta", "id"))
        case Some(id) =>
          ps.name = List(id)
      }
    }

    ps
  }

  def typesFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, StructSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (typeName, body) =>
      Identifier.checkIdentifierSource(typeName, "type", path ++ List(typeName))
      typeName -> StructSpec.fromYaml(body, path ++ List(typeName), metaDef)
    }
  }

  def inputsFromYaml(src: Any, path: List[String], metaDef: MetaSpec): Map[String, InputSpec] = {
    val srcMap = ParseUtils.asMapStr(src, path)
    srcMap.map { case (inputName, body) =>
      Identifier.checkIdentifierSource(inputName, "inputs", path ++ List(inputName))
      inputName -> InputSpec.fromYaml(body, path ++ List(inputName), metaDef)
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

  def fromYaml(src: Any): ProtocolSpec = fromYaml(src, List(), MetaSpec.OPAQUE)
}
