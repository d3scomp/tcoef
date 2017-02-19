package rcrs.comm

import scodec.bits.BitVector
import scodec.codecs._

/**
  * Message sent from brigade to central agent
  */
case class FireBrigadeToInitiator(x: Int, y: Int, mirrorState: Int) extends Message

object FireBrigadeToInitiator {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.FIRE_BRIGADE_TO_INITIATOR.id, Message.MessageTypeBits))) ::
    ("state" | uint(4)) ::
    ("x" | uint(20)) :: // TODO - create codec for position
    ("y" | uint(20))
  }.as[FireBrigadeToInitiator]
}
