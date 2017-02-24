package rcrs.scenario

import rcrs.comm._
import rcrs.traits.{WithEntityID, RCRSConnectorTrait}
import rescuecore2.log.Logger
import rescuecore2.worldmodel.EntityID
import tcof.traits.map2d.Position
import tcof.{WithActionsInComponent, Model, Component}

trait PositionRegistrySupport {
  this: Model with RCRSConnectorTrait =>

  trait PositionSending {
    this: Component with WithEntityID =>

    val SendPosition = State

    actuation {
      states.selectedMembers.foreach {
        case SendPosition => sendPositionAction()
        case _ =>
      }
    }

    def sendPositionAction() = {
      val x = agent.getPosition.x.toInt
      val y = agent.getPosition.y.toInt
      val message = Message.encode(new CurrentPosition(x, y))
      Logger.info(s"Sending position of component: ${id}, x=$x y=$y")
      agent.sendSpeak(time, Constants.TO_STATION, message)
    }
  }

  // TODO - make trait more generic? Use type from WithId?
  trait PositionReceiver {
    this: WithActionsInComponent =>

    // mapping EntityID -> PositionAware for fast lookup
    // initialization is delayed to preactions (components field is assigned later)
    var positionRegistry: Map[EntityID, PositionAware] = null

    sensing {
      if (positionRegistry == null) {
        positionRegistry = components.collect{case x: Component with PositionAware with WithEntityID => x}
          .map{x => x.id -> x}.toMap
      }

      sensing.messages.foreach{
        case (CurrentPosition(x: Int, y: Int), message) =>
          val senderId = message.getAgentID

          if (positionRegistry.contains(senderId)) {
            Logger.info(s"Updating position of component: $senderId to x=$x y=$y")
            positionRegistry(senderId).position = Position(x, y)
          } else {
            Logger.info(s"Component $senderId not found in positionRegistry")
          }

        case _ =>
      }
    }
  }
}
