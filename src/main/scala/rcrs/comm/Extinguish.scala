package rcrs.comm

import rescuecore2.worldmodel.EntityID
import scodec.bits.BitVector
import scodec.codecs._

case class Extinguish(id: EntityID) extends Message

object Extinguish {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.EXTINGUISH.id, Message.MessageTypeBits))) ::
    ("id" | Message.entityIDCodec)
  }.as[Extinguish]
}