package rcrs.comm

import rescuecore2.worldmodel.EntityID
import scodec.bits.BitVector
import scodec.codecs._

case class FireBrigadeStatus(waterLevel: Int) extends Message

object FireBrigadeStatus {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.FIRE_BRIGADE_STATUS.id, Message.MessageTypeBits))) ::
    ("waterLevel" | uint(16)) // allowed range by rcrs (5000 - 50000)
  }.as[FireBrigadeStatus]
}