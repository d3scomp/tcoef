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
    this: MobileUnitComponent#MobileUnit =>

    var counter = 0
    val period = 2

    // TODO - is sending a position an action? Should it have its own state?
    // In rcrs can agent perform in one step both
    // - sending a message and e.g. moving to some location
    val SendPosition = State

    preActions {
      counter = (counter + 1) % period
    }

    constraints(
      SendPosition <-> (counter == 0 && shortId != ShortIdUndefined)
    )

    actions {
      // TODO - why do I iterate selectedMembers? (Also in Registration)
      states.selectedMembers.foreach {
        case SendPosition =>
          val x = agent.getPosition.x.toInt
          val y = agent.getPosition.y.toInt
          val message = Message.encode(new CurrentPosition(x, y))
          Logger.info(s"Sending position of component: $shortId, x=$x y=$y")
          agent.sendSpeak(time, Constants.TO_STATION, message)

        case _ =>
      }
    }
  }

  // TODO - make trait more generic? Use type from WithId?
  trait PositionRegistry {
    this: WithActionsInComponent with RegistrationSupport#Registrator =>

    // initialization is delayed to preactions (components field is assigned later in )
    var positionRegistry: Map[EntityID, PositionAware] = null

    preActions {
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
