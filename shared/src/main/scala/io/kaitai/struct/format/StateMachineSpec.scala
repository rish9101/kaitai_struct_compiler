package io.kaitai.struct.format

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{UserType, UserTypeInstream}

case class StateMachineSpec(
  path: List[String],
  states: Map[String, Map[String, UserType]]
)

object StateMachineSpec{
    val EMPTY = StateMachineSpec(List(), Map())
    def fromYaml(
        srcMap: Map[String, Any],
        path:  List[String]
    ): StateMachineSpec = {
        // TODO populate state machine
        val states = srcMap.map { case (src, dsts) =>
            src -> ParseUtils.asMapStrStr(dsts, path ++ List(src)).map { case (dst, typeStr) =>
                val dtl = DataType.classNameToList(typeStr)
                dst -> UserTypeInstream(dtl, None, List())
            }
        }
        StateMachineSpec(path, states)
    }

}
