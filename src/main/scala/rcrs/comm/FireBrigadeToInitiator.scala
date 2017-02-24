package rcrs.comm

import rcrs.scenario.ProtectScenario.FireBrigadeStatic.MirrorState.MirrorState
import rcrs.traits.map2d.{BuildingStatus, RCRSNodeStatus}
import rescuecore2.worldmodel.EntityID
import scodec._
import scodec.bits.{BitVector, _}
import scodec.codecs._
import tcof.traits.map2d.Position

/**
  * Message sent from brigade to central agent
  */
case class FireBrigadeToInitiator(mirrorState: MirrorState, position: Position, currentAreaID: EntityID, statusMap: Map[Int, RCRSNodeStatus]) extends Message

object FireBrigadeToInitiator {
  val buildingStatusCodec = {
    (constant(bin"01")) ::
      ("temperature" | uint(10)) ::
      ("brokenness" | uint8) ::
      ("fieryness" | uint4)
  }.as[BuildingStatus].upcast[RCRSNodeStatus]

  val closeIdxCodec = uint(16)

  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.FIRE_BRIGADE_TO_INITIATOR.id, Message.MessageTypeBits))) ::
    ("state" | Message.fireBrigadeStateCodec) ::
    ("position" | Message.positionCodec) ::
    ("currentAreaId" | Message.entityIDCodec) ::
    ("statusMap" | listOfN(uint(6), ("closeIdx" | closeIdxCodec) ~ buildingStatusCodec).xmap[Map[Int, RCRSNodeStatus]](_.toMap, _.toList))
  }.as[FireBrigadeToInitiator]
}
