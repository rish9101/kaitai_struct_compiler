package io.kaitai.struct.languages.components

import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType._
import io.kaitai.struct.datatype.FixedEndian
import io.kaitai.struct.exprlang.Ast
import io.kaitai.struct.exprlang.Ast.expr
import io.kaitai.struct.format._

import scala.collection.mutable.ListBuffer

trait EveryInitIsExpression extends LanguageCompiler with ObjectOrientedLanguage with EveryReadIsExpression {
  override def attrInit(attr: AttrLikeSpec, id: Identifier, defEndian: Option[FixedEndian]): Unit = {
        val io = normalIO
        // attrInit2(id, attr.dataType, io, attr.cond.repeat, false, defEndian)

        defineReadStart(id, attr.dataType, attr.switchOnValue)
    
    }

  def attrInit2(
        id: Identifier,
        dataType: DataType,
        io: String,
        rep: RepeatSpec,
        isRaw: Boolean,
        defEndian: Option[FixedEndian],
        exprTypeOpt: Option[DataType] = None
    ): Unit = {
    //     dataType match {
    //   case t: UserType =>
    //     defineReadStart(id, t)
    //   case t: BytesType =>
    //     defineReadStart(id, t)
    //   case st: SwitchType =>
    //     val isNullable = if (switchBytesOnlyAsRaw) {
    //       st.isNullableSwitchRaw
    //     } else {
    //       st.isNullable
    //     }
    //     defineReadStart(id, st)
    //   case t: StrFromBytesType =>
    //     defineReadStart(id, t)
    //   case t: EnumType =>
    //     defineReadStart(id, t)
    //   case _ =>
    //     defineReadStart(id, dataType)
    // }
  }

  def defineSwitchMap(switchValSpec: Option[SwitchValueSpec]): String

  def defineSwitchOn(switchValSpec: Option[SwitchValueSpec]) : String

}
