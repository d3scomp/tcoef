package rcrs.comm

import rcrs.traits.map2d.{BuildingStatus, RCRSNodeStatus, RoadStatus}
import rescuecore2.worldmodel.EntityID
import scodec._
import scodec.bits._
import scodec.codecs._

case class ExplorationStatus(currentAreaId: EntityID, statusMap: Map[Int, RCRSNodeStatus]) extends Message

object ExplorationStatus {
  val closeIdxCodec = uint(7)

  val roadStatusCodec = {
    (constant(bin"00")) ::
    ("dummy" | int8)
  }.as[RoadStatus].upcast[RCRSNodeStatus]

  val buildingStatusCodec = {
    (constant(bin"01")) ::
    ("temperature" | uint(10)) ::
    ("brokenness" | uint8) ::
    ("fieryness" | uint4)
  }.as[BuildingStatus].upcast[RCRSNodeStatus]

  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.EXPLORATION_STATUS.id, Message.MessageTypeBits))) ::
    ("currentAreaId" | Message.entityIDCodec) ::
    ("statusMap" | listOfN(uint(6), ("closeIdx" | closeIdxCodec) ~ choice(roadStatusCodec, buildingStatusCodec)).xmap[Map[Int, RCRSNodeStatus]](_.toMap, _.toList))
  }.as[ExplorationStatus]
}