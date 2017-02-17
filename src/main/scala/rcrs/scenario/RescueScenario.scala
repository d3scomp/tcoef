package rcrs.scenario

import rcrs.{FireBrigadeAgent, comm, ScalaAgent}
import rcrs.comm._
import rescuecore2.log.Logger
import rescuecore2.worldmodel.EntityID
import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.{WithEntityID, RCRSConnectorTrait}
import rcrs.traits.map2d.RCRSNodeStatus


class RescueScenario(scalaAgent: ScalaAgent) extends Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] with MobileUnitComponent with CentralUnitComponent with ObservationSupport with PositionRegistrySupport {
  this.agent = scalaAgent

  class FireBrigade(val id: EntityID, position: Position) extends MobileUnit(position) {
    var assignedFire: Position = _ // assigned by message send by some ensemble
    var waterLevel: Int = _
    var selectedForExtinguishing: Boolean = false

    val Refilling = State // includes heading to refill point
    val Extinguishing = State // includes heading to fire and waiting for selection near fire
    val SendFireBrigadeStatus = State
    val FireFighting = StateAnd(StateOr(Refilling, Extinguishing), Observation, SendPosition, SendFireBrigadeStatus) // added Observation to propagate knowledge to ensemble
    val Idle = State // TODO - model Idle explicitly? Or represent it by fact that Firefighting is not selected?
    val Operational = StateOr(FireFighting, Idle) // top-level state

    constraints(
      FireFighting <-> (assignedFire != null) &&
      Refilling -> (refillingAtRefillPlace || tankEmpty) &&
      Extinguishing -> (!tankEmpty)
    )

    preActions {
      sensing.messages.foreach{
        // Knowledge propagated from component to ensemble:
        // - buildings on fire - in ObservationSupport
        // - current position of agent - in PositionRegistrySupport
        // - water level - add message?
        // - switched from extinguishing to refilling - deduced from water level
        //
        // From ensemble to component:
        // - selectedForExtinguishing - add message?
        // - fire extinguished - detected from world model, passed by ObservationSupport

        case (Extinguish(id), _) if id == agent.getID =>
          Logger.info(s"Extinguish received by agent ${agent.getID}")

        case _ =>
      }
    }

    actions {
      states.selectedMembers.foreach {
        case Extinguishing =>
          extinguishAction
        case Refilling =>
          refillAction
        case SendFireBrigadeStatus =>
          sendStatusAction
        case _ =>
      }
    }

    def tankEmpty: Boolean = getWater == 0

    def refillingAtRefillPlace: Boolean = ???

    def refillAction = {
      if (refillingAtRefillPlace) {
        // TODO - rest? This means that brigade cannot extinguish during refill (which is possible in rcrs)
        agent.sendRest(time)
      } else {
        // TODO - move to refill point
      }
      ???
    }

    def extinguishAction = {
      if (isNearAssignedFire) {
        if (selectedForExtinguishing) {
          // TODO - extinguish
        } else {
          // wait
          agent.sendRest(time)
        }
      } else {
        // TODO - move brigade to assigned fire
      }
      ???
    }

    def sendStatusAction = {
      val waterLevel: Int = agentAs[rescuecore2.standard.entities.FireBrigade].me.getWater
      val message = Message.encode(new FireBrigadeStatus(waterLevel))
      Logger.info(s"Sending status of firebrigade: ${id}, water=$waterLevel")
      agent.sendSpeak(time, Constants.TO_STATION, message)
    }

    def isNearAssignedFire: Boolean = {
      ???
    }

    def getWater: Int = agentAs[rescuecore2.standard.entities.FireBrigade].me.getWater
    def maxWater: Int = agent.asInstanceOf[FireBrigadeAgent].maxWater
  }

  class FireStation(entityID: EntityID, position: Position) extends CentralUnit(position) /*with ObservationReceiver with PositionReceiver*/ {
    var fireBrigadeRegistry: Map[EntityID, FireBrigade] = null

    preActions {
      if (fireBrigadeRegistry == null) {
        fireBrigadeRegistry = components.collect{ case fb: FireBrigade => fb}
          .map{ fb => fb.id -> fb}.toMap
      }

      sensing.messages.foreach{
        case (FireBrigadeStatus(waterLevel), message) =>
          val fb = fireBrigadeRegistry(message.getAgentID)
          fb.waterLevel = waterLevel

        case _ =>
      }
    }
  }

  class System extends RootEnsemble /* TODO - will extend just Ensamble */{

    membership(
      ???
      //explorationTeams.map(_.fireBrigades).allDisjoint
      // && explorationTeams.map(_.ambulances).allDisjoint
      // && explorationTeams.map(_.police).allDisjoint
    )

    actions {
      // nulls all assigned zones
      // TODO - the information about zones should be contained in ExplorationTeam ensamble
      // this leaks information about zones into parent ensamble
      //components.select[MobileUnit].map(_.areaExplorationAssignedZone = null)
    }
  }

  val rootEnsemble = root(new System)

  // Maintains a queue of available brigades that are able to extinguish given fire.
  // - When the brigade that extinguish the fire runs out of water, the ensemble
  //   directs next brigade to start extinguishing.
  //   Brigade with empty tank automatically heads to the nearest refill station and
  //   refills its tank.
  class ExtinguishingCoordination(val fire: Position) extends Ensemble {

    // membership
    // - ensemble chooses brigade which will extinguish the fire
    val brigades = role("extinguishingCoordination", components.select[FireBrigade])

    var currentlyExtinguishing: FireBrigade = _
    val brigadesAvailable = List[FireBrigade]()
    val brigadesRefilling = List[FireBrigade]()

    membership(
      brigades.all(_.assignedFire == fire)
    )

    // utility function not needed - TODO - can be ommited?
    // utility {
    // }

    actions {
      checkCurrentlyExtinguishingBrigade
      selectBrigadeForExtinguishingIfNeeded
    }

    def checkCurrentlyExtinguishingBrigade: Unit = {
      if (currentlyExtinguishing != null && currentlyExtinguishing.waterLevel == 0) {
        currentlyExtinguishing = null
      }
    }

    def selectBrigadeForExtinguishingIfNeeded: Unit = {
      if (currentlyExtinguishing == null) {
        // select new brigade


        // send message
      }
    }
  }

}