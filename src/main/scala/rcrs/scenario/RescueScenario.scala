package rcrs.scenario

import rcrs.{comm, ScalaAgent}
import rcrs.comm._
import rescuecore2.log.Logger
import rescuecore2.worldmodel.EntityID
import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.{WithEntityID, RCRSConnectorTrait}
import rcrs.traits.map2d.RCRSNodeStatus


class RescueScenario(scalaAgent: ScalaAgent) extends Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] with ObservationSupport {

  this.agent = scalaAgent

  class FireBrigade(entityID: EntityID, _position: Position) extends Component with WithEntityID with Observation {
    val id = entityID
    var position: Position = _position
    var assignedFire: Position = _ // assigned by message send by some ensemble
    var selectedForExtinguishing: Boolean = _

    val Refilling = State // includes heading to refill point
    val Extinguishing = State // includes heading to fire and waiting for selection near fire
    val FireFighting = StateAnd(StateOr(Refilling, Extinguishing), Observation) // added Observation to propagate knowledge to ensemble
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
        // - current position of agent - TODO add message or use just areaId from ExplorationStatus?
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
        case _ =>
      }
    }

    def tankEmpty: Boolean = ???

    def refillingAtRefillPlace: Boolean = ???

    def refillAction = {
      if (refillingAtRefillPlace) {
        // TODO - rest? This means that brigade cannot extinguish during refill (which is possible in rcrs)
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
          // TODO - wait
        }
      } else {
        // TODO - move brigade to assigned fire
      }
      ???
    }

    def isNearAssignedFire: Boolean = {
      ???
    }

  }

  class FireStation(entityID: EntityID, _position: Position) extends Component with ObservationReceiver {
    //val id = entityID
    //name(s"FireStation $entityID")

    preActions {
//      sensing.messages.foreach{
//        case (Arrived(), _) =>
//        case (NoWater(), _) =>
//        case (FireExtinguished(), _) =>
//        case _ =>
//      }
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
  // - When the brigade that extinguish the fire runs out of water, it sends a message
  //   to ensemble and ensemble directs next brigade to start extinguishing.
  //   Brigade automatically heads to the nearest refill station and refills its tank.
  // - When the brigade returns after refilling, it sends a message that it is
  //   available
  class ExtinguishingCoordination(val fire: Position) extends Ensemble {

    // membership
    // - ensemble chooses brigade which will extinguish the fire
    val brigades = role("fireBrigadesNeedingRefill", components.select[FireBrigade])

    var currentlyExtinguishing: FireBrigade = _
    val brigadesAvailable = List[FireBrigade]()
    val brigadesRefilling = List[FireBrigade]()

    var selectBrigadeForExtinguishing = false

    membership(
      brigades.all(_.assignedFire == fire)
    )

    // utility function not needed - TODO - can be ommited?
    // utility {
    // }

    actions {
      selectBrigadeForExtinguishingIfNeeded
    }

    def selectBrigadeForExtinguishingIfNeeded: Unit = {
      if (selectBrigadeForExtinguishing && !brigadesAvailable.isEmpty) {
        // TODO - send message to brigade - but messagees are received
        // by coordinator. Coordinator just sets a flag?

        currentlyExtinguishing = brigadesAvailable.head
        brigadesAvailable.drop(1)

        selectBrigadeForExtinguishing = false
      }
    }
  }

}