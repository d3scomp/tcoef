package rcrs.comm

import scodec.bits.BitVector
import scodec.codecs._
import rescuecore2.worldmodel.EntityID

/**
  * Message sent from central agent to brigade
  */
case class InitiatorToFireBrigade(receiverId: EntityID, mirrorState: Int, asignedFireLocation: EntityID, assignedFireLocationDefined: Boolean) extends Message

object InitiatorToFireBrigade {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.INITIATOR_TO_FIRE_BRIGADE.id, Message.MessageTypeBits))) ::
    ("receiverId" | Message.entityIDCodec) ::
    ("mirrorState" | uint(4)) ::
    ("assignedFireLocation" | Message.entityIDCodec) ::
    ("assignedFireLocationDefined" | bool) // TODO use optional codec with assignedFireLocation
  }.as[InitiatorToFireBrigade]
}
