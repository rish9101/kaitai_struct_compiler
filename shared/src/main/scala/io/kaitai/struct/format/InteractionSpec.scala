package io.kaitai.struct.format

sealed trait InteractionSpec

case object TransmitInteraction extends InteractionSpec
case object ReceiveInteraction  extends InteractionSpec
case object DelayInteraction  extends InteractionSpec
case object NonInteraction extends InteractionSpec

object InteractionSpec{

    def fromYaml(
        srcMap: Map[String, Any],
        path:  List[String]
    ): InteractionSpec = {
        val packetType = ParseUtils.getOptValueStr(srcMap, "packet-type", path)

        packetType match {
            case None => return NonInteraction
            case Some(interaction) => {
                interaction match {
                    case "transmit" => return TransmitInteraction
                    case "receive" => return ReceiveInteraction
                    case "delay" => return DelayInteraction
                    case _ => return NonInteraction
                }

            }
        }

    }

}
