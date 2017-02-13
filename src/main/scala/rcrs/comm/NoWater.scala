package rcrs.comm

import scodec.bits.BitVector
import scodec.codecs._

case class NoWater() extends Message

object ToRefillNoWater {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.NO_WATER.id, Message.MessageTypeBits)))
  }.xmap((x: Unit) => new NoWater(), (x: NoWater) => {} )
}