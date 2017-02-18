package rcrs.comm

import scodec.bits.BitVector
import scodec.codecs._

/**
  * Message sent from brigade to central agent
  */
case class FireBrigadeToInitiator(x: Int, y: Int, /* waterLevel: Int, */ mirrorState: Int) extends Message

object FireBrigadeToInitiator {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.FIRE_BRIGADE_TO_INITIATOR.id, Message.MessageTypeBits))) ::
    ("state" | uint(4)) ::
    ("x" | uint(20)) ::
    ("y" | uint(20))
    //("waterLevel" | uint(16)) // allowed range by rcrs (5000 - 50000)
  }.as[FireBrigadeToInitiator]
}
