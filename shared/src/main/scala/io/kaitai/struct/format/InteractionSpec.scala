package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{ConstraintMap, UserTypeInstream}
import io.kaitai.struct.exprlang.Expressions

sealed trait InteractionType

case object TransmitInteraction extends InteractionType
case object ReceiveInteraction extends InteractionType
case class DelayInteraction(interval: Int) extends InteractionType

case class InteractionSpec(
  path: List[String],
  id: Identifier,
  dataType: DataType,
  cond: ConditionalSpec = ConditionalSpec(None, NoRepeat),
  valid: Option[ValidationSpec] = None,
  doc: DocSpec = DocSpec.EMPTY,
  action: InteractionType,
  exports: Option[ValidationSpec]
) extends AttrLikeSpec with MemberSpec with ExportSpec {
  override def isLazy = false
}

object InteractionSpec {
  val LEGAL_KEYS = Set(
    "id",
    "doc",
    "doc-ref",
    "type",
    "if",
    "constraints",
    "action",
    "exports",
    "delay"
  )

  def fromYaml(src: Any, path: List[String], metaDef: MetaSpec, idx: Int): InteractionSpec = {
    val srcMap = ParseUtils.asMapStr(src, path)
    val id = ParseUtils.getOptValueStr(srcMap, "id", path) match {
      case Some(idStr) =>
        try {
          NamedIdentifier(idStr)
        } catch {
          case _: InvalidIdentifier =>
            throw YAMLParseException.invalidId(idStr, "interaction", path ++ List("id"))
        }
      case None => NumberedIdentifier(idx)
    }
    fromYaml(srcMap, path, metaDef, id)
  }

  def fromYaml(srcMap: Map[String, Any], path: List[String], metaDef: MetaSpec, id: Identifier): InteractionSpec = {
    try {
      fromYaml2(srcMap, path, metaDef, id)
    } catch {
      case (epe: Expressions.ParseException) =>
        throw YAMLParseException.expression(epe, path)
    }
  }

  def fromYaml2(srcMap: Map[String, Any], path: List[String], metaDef: MetaSpec, id: Identifier): InteractionSpec = {
    val doc = DocSpec.fromYaml(srcMap, path)
    val ifExpr = ParseUtils.getOptValueExpression(srcMap, "if", path)
    val constraints = srcMap.get("constraints").map(parseConstraintSpec(_, path ++ List("constraints")))
    val actionStr = ParseUtils.getValueStr(srcMap, "action", path ++ List("action"))
    val exportConstraints = srcMap.get("exports").map(parseConstraintSpec(_, path ++ List("exports")))
    val delay = ParseUtils.getOptValueInt(srcMap, "delay", path ++ List("delay"))

    val typStr = ParseUtils.getValueStr(srcMap, "type", path ++ List("type"))
    val dtl = DataType.classNameToList(typStr)
    val dataType: DataType = UserTypeInstream(dtl, None, List())

    val action = actionStr match {
      case "transmit" | "send" => TransmitInteraction
      case "receive" => ReceiveInteraction
      case "delay" => DelayInteraction(delay.get)
    }

    val valid: Option[ValidationSpec] = constraints match {
      case Some(cmap) => Some(ValidationAllOf(cmap))
      case _ => None
    }

    val exports = exportConstraints match {
      case Some(cmap) => Some(ValidationAllOf(cmap))
      case _ => None
    }

    ParseUtils.ensureLegalKeys(srcMap, LEGAL_KEYS, path)

    InteractionSpec(path, id, dataType, ConditionalSpec(ifExpr, NoRepeat), valid, doc, action, exports)
  }

  def parseConstraintSpec(c: Any, path: List[String]): ConstraintMap = {
    c match {
      case constraintMap: Map[Any, Any] =>
        var cMap = ParseUtils.anyMapToStrMap(constraintMap, path)
        ConstraintMap.fromYaml(cMap, path)
      case _ =>
        throw new YAMLParseException(s"unable to parse constraints", path)
    }
  }

}
