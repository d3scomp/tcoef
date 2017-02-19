package rcrs.comm

import rcrs.scenario.ProtectScenario
import rescuecore2.worldmodel.EntityID
import scodec.Attempt.{Failure, Successful}
import scodec.bits._
import scodec.codecs._
import tcof.traits.map2d.Position

abstract class Message

object Message {
  val MessageTypeBits = 4

  object MessageType extends Enumeration {
    type MessageType = Value

    val REG_REQUEST = Value(0)
    val REG_RESPONSE = Value(1)
    val EXPLORATION_STATUS = Value(2)
    val CURRENT_POSITION = Value(3)
    val EXTINGUISH = Value(4)
    val FIRE_BRIGADE_STATUS = Value(5)
    val FIRE_BRIGADE_TO_INITIATOR = Value(6)
    val INITIATOR_TO_FIRE_BRIGADE = Value(7)
  }

  val shortIdCodec = uint4
  val entityIDCodec = int32.xmap((id: Int) => new EntityID(id), (id: EntityID) => id.getValue)
  val positionCodec = (double :: double).as[Position]
  val fireBrigadeStateCodec = enumerated(uint4, ProtectScenario.FireBrigadeStatic.MirrorState)


  val codec = choice(
    RegRequest.codec.upcast[Message],
    RegResponse.codec.upcast[Message],
    ExplorationStatus.codec.upcast[Message],
    CurrentPosition.codec.upcast[Message]
  )

  def decode(bytes: Array[Byte]): Message = {
    val bits = BitVector(bytes)

    codec.decodeValue(bits) match {
      case Successful(msg) => msg
      case Failure(_) => null
    }
  }

  def encode(msg: Message): Array[Byte] = {
    codec.encode(msg).require.toByteArray
  }
}