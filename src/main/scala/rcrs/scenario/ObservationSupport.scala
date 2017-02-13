package rcrs.scenario

import tcof.traits.map2d.{Map2DTrait, Node}
import rcrs.comm.{Constants, ExplorationStatus, Message}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.{BuildingStatus, RCRSNodeStatus, RoadStatus}
import rescuecore2.standard.entities.{StandardPropertyURN, Area, Building, Road}
import tcof.Model

import scala.collection.JavaConverters._
import scala.collection.mutable

trait ObservationSupport {
  this: Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] =>

  /**
    * Observes and records changes in its close vicinity. It checks the "changes" variable, which contains
    * what the agent has seen. Based on this, it updates the map and sends the changes to the central station.
    */
  trait Observation {
    this: MobileUnitComponent#MobileUnit =>

    val Observation = State

    actions {
      states.selectedMembers.foreach {
        case Observation => doObservation()
        case _ =>
      }
    }

    def doObservation() {
      val referenceNode = map.toNode(agent.currentAreaId)
      val statusChanges = mutable.Map.empty[Node[RCRSNodeStatus], RCRSNodeStatus]

      val changes = sensing.changes

      for (entityId <- changes.getChangedEntities.asScala) {

        agent.model.getEntity(entityId) match {
          case area: Area =>
            val changedNode = map.toNode(area.getID)

            area match {
              case road: Road => statusChanges += changedNode -> RoadStatus(42 /* TODO */)
              case building: Building =>
                val temperature = changes.getChangedProperty(entityId, StandardPropertyURN.TEMPERATURE.toString).getValue.asInstanceOf[Int]
                val brokenness = changes.getChangedProperty(entityId, StandardPropertyURN.BROKENNESS.toString).getValue.asInstanceOf[Int]
                statusChanges += changedNode -> BuildingStatus(temperature, brokenness)
            }

          case _ =>
        }
      }

      // TODO - isn't nodeStatus redundant? Informarmation about temperature, brokeness should be available in agent's
      // world model
      map.nodeStatus ++= statusChanges

      val statusMap = statusChanges.collect {
        case (node, status) => map.closeAreaIDs(agent.currentAreaId).byAreaId(map.toArea(node).getID) -> status
      }.toMap
      val msg = new ExplorationStatus(agent.currentAreaId, statusMap)
      agent.sendSpeak(time, Constants.TO_STATION, Message.encode(msg))
    }
  }

}
