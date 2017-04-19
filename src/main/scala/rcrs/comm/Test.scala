package rcrs.comm

import rcrs.scenario.MirrorState
import rcrs.traits.map2d.{BuildingStatus, RoadStatus}
import rescuecore2.worldmodel.EntityID
import scodec.bits.BitVector
import tcof.traits.map2d.Position

object Test {
  def main(args: Array[String]): Unit = {
    val bytes1 = Message.encode(ExplorationStatus(new EntityID(42), Map(1 -> RoadStatus(42), 2 -> RoadStatus(14))))
    println(BitVector(bytes1))

    val msg1 = Message.decode(bytes1)
    println(msg1)

    val bytes2 = Message.encode(RegRequest())
    println(BitVector(bytes2))

    val msg2 = Message.decode(bytes2)
    println(msg2)

    val bytes3 = Message.encode(RegResponse(new EntityID(12),5))
    println(BitVector(bytes3))

    val msg3 = Message.decode(bytes3)
    println(msg3)

    val bytes4 = Message.encode(FireBrigadeToInitiator(
      MirrorState.IdleMirror,
      Position(416655.0,144450.0),
      new EntityID(2922),
      Map(
        29952 -> BuildingStatus(0,13,0),
        19787 -> BuildingStatus(0,100,0)
      )
    ))
    println(BitVector(bytes4))

    val msg4 = Message.decode(bytes4)
    println(msg4)


  }
}