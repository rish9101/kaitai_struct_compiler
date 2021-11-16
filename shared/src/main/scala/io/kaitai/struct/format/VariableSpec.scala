package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.exprlang.{Ast, Expressions}

case class VariableSpec(
  path: List[String],
  value: Option[Ast.expr],
  var dataType: Option[DataType]
)

object VariableSpec {
  val LEGAL_KEYS_VARIABLE = Set(
    "value",
    "type"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec, id: NamedIdentifier): VariableSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)

    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS_VARIABLE, path, Some("variable"))

    val valueExpr = ParseUtils.getOptValueExpression(srcMap, "value", path)
    val typeStr = ParseUtils.getOptValueStr(srcMap, "type", path)

    val dataType = typeStr match {
      case Some(_) =>
        val fakeYamlAttrArgs = YamlAttrArgs(
          None, false,
          None, None, false, true, true, None,
          None, None, None, None, None, None
        )
        Some(DataType.fromYaml(typeStr, path, metaDef, fakeYamlAttrArgs))
      case _ =>
        None
    }
    VariableSpec(path, valueExpr, dataType)
  }
}
