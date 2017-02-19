package rcrs.comm

import rcrs.scenario.ProtectScenario.FireBrigadeStatic.MirrorState.MirrorState
import scodec.bits.BitVector
import scodec.codecs._
import tcof.traits.map2d.Position

/**
  * Message sent from brigade to central agent
  */
case class FireBrigadeToInitiator(mirrorState: MirrorState, position: Position) extends Message

object FireBrigadeToInitiator {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.FIRE_BRIGADE_TO_INITIATOR.id, Message.MessageTypeBits))) ::
    ("state" | Message.fireBrigadeStateCodec) ::
    ("position" | Message.positionCodec)
  }.as[FireBrigadeToInitiator]
}
