package rcrs.comm

import scodec.bits.BitVector
import scodec.codecs._

case class CurrentPosition(x: Int, y: Int) extends Message

object CurrentPosition {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.CURRENT_POSITION.id, Message.MessageTypeBits))) ::
    ("x" | uint(20)) ::
    ("y" | uint(20))
  }.as[CurrentPosition]
}