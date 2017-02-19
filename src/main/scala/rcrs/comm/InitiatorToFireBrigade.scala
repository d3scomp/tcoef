package rcrs.comm

import rcrs.scenario.ProtectScenario.FireBrigadeStatic.MirrorState.MirrorState
import rescuecore2.worldmodel.EntityID
import scodec.bits.BitVector
import scodec.codecs._

/**
  * Message sent from central agent to brigade
  */
case class InitiatorToFireBrigade(receiverId: EntityID, state: MirrorState, asignedFireLocation: Option[EntityID]) extends Message

object InitiatorToFireBrigade {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.INITIATOR_TO_FIRE_BRIGADE.id, Message.MessageTypeBits))) ::
    ("receiverId" | Message.entityIDCodec) ::
    ("state" | Message.fireBrigadeStateCodec) ::
    ("assignedFireLocation" | optional(bool, Message.entityIDCodec))
  }.as[InitiatorToFireBrigade]
}
