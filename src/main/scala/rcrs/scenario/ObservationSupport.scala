package rcrs.scenario

import rescuecore2.worldmodel.EntityID
import tcof.traits.map2d.{Map2DTrait, Node}
import rcrs.comm._
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.{BuildingStatus, RCRSNodeStatus, RoadStatus}
import rescuecore2.standard.entities.{StandardPropertyURN, Area, Building, Road}
import tcof.{Component, Model}

import scala.collection.JavaConverters._
import scala.collection.mutable

trait ObservationSupport {
  this: Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] =>

  /**
    * Observes and records changes in its close vicinity. It checks the "changes" variable, which contains
    * what the agent has seen. Based on this, it updates the map and sends the changes to the central station.
    */
  trait Observation {
    this: Component =>

    val Observation = State

    actuation {
      states.selectedMembers.foreach {
        case Observation => doObservation()
        case _ =>
      }
    }

    def doObservation() {
      val referenceNode = map.toNode(agent.currentAreaId)
      val statusChanges = mutable.Map.empty[Node[RCRSNodeStatus], RCRSNodeStatus]

      val changes = sensed.changes

      for (entityId <- changes.getChangedEntities.asScala) {

        agent.model.getEntity(entityId) match {
          case area: Area =>
            val changedNode = map.toNode(area.getID)

            area match {
              case road: Road =>
                statusChanges += changedNode -> RoadStatus(42 /* TODO */)

              case building: Building =>
                val temperature = changes.getChangedProperty(entityId, StandardPropertyURN.TEMPERATURE.toString).getValue.asInstanceOf[Int]
                val brokenness = changes.getChangedProperty(entityId, StandardPropertyURN.BROKENNESS.toString).getValue.asInstanceOf[Int]
                val fieryness = changes.getChangedProperty(entityId, StandardPropertyURN.FIERYNESS.toString).getValue.asInstanceOf[Int]
                statusChanges += changedNode -> BuildingStatus(temperature, brokenness, fieryness)
            }

          case _ =>
        }
      }

      // TODO - isn't nodeStatus redundant? Informarmation about temperature, brokeness
      // should be available in agent's world model
      map.nodeStatus ++= statusChanges

      val statusMap = statusChanges.collect {
        case (node, status) => map.closeAreaIDs(agent.currentAreaId).byAreaId(map.toArea(node).getID) -> status
      }.toMap
      val msg = new ExplorationStatus(agent.currentAreaId, statusMap)
      agent.sendSpeak(time, Constants.TO_STATION, Message.encode(msg))
    }
  }

  trait ObservationReceiver {
    this: Component =>

    sensing {
      sensed.messages.foreach {
        case (ExplorationStatus(currentAreaId, statusMap), speak) =>
          updateWorldInfo(statusMap)

        case _ =>
      }
    }

    def updateWorldInfo(statusMap: Map[Int, RCRSNodeStatus]) = {
      for ((id, nodeStatus) <- statusMap) {
        nodeStatus match {
          case BuildingStatus(temperature, brokenness, fieryness) =>
            val entityID = new EntityID(id)
            map.toNode(entityID).status = nodeStatus

          case _ =>
        }
      }
    }
  }
}
