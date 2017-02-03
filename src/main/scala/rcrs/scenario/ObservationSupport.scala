package rcrs.scenario

import tcof.traits.map2d.{Map2DTrait, Node}
import rcrs.comm.{Constants, ExplorationStatus, Message}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.{BuildingStatus, RCRSNodeStatus, RoadStatus}
import rescuecore2.standard.entities.{Area, Building, Road}
import tcof.Universe

import scala.collection.JavaConverters._
import scala.collection.mutable

trait ObservationSupport {
  this: Universe with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] =>

  /**
    * Observes and records changes in its close vicinity. It checks the "changes" variable, which contains what the agent has seen. Based on this, it upddates the map and sends the changes to the central station.
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
              case building: Building => statusChanges += changedNode -> BuildingStatus(changes.getChangedProperty(entityId, "urn:rescuecore2.standard:property:temperature").getValue.asInstanceOf[Int], changes.getChangedProperty(entityId, "urn:rescuecore2.standard:property:brokenness").getValue.asInstanceOf[Int])
            }

          case _ =>
        }
      }

      map.nodeStatus ++= statusChanges

      val msg = new ExplorationStatus(agent.currentAreaId, statusChanges.collect { case (node, status) => map.closeAreaIDs(agent.currentAreaId).byAreaId(map.toArea(node).getID) -> status }.toMap)
      agent.sendSpeak(time, Constants.TO_STATION, Message.encode(msg))
    }
  }

}
