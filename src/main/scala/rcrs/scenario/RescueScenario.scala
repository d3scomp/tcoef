package rcrs.scenario

import rcrs.ScalaAgent
import rcrs.comm._
import rescuecore2.log.Logger
import rescuecore2.worldmodel.EntityID
import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.{WithEntityID, RCRSConnectorTrait}
import rcrs.traits.map2d.RCRSNodeStatus


class RescueScenario(scalaAgent: ScalaAgent) extends Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] {

  this.agent = scalaAgent

  class FireBrigade(entityID: EntityID, _position: Position) extends Component with WithEntityID {
    val id = entityID
    var position: Position = _position
    var fireToExtinguish: Position = _

    val Extinguishing = State
    val OutOfWater = State
    val ToRefill = State // + refill
    val ToFire = State
    val Ready = State

    // no constraints so far - TODO - can be ommited?
//    constraints(
//    )

    preActions {
      sensing.messages.foreach{
        case (Extinguish(id), _) if id == agent.getID =>
          Logger.info(s"Extinguish received by agent ${agent.getID}")
          shouldExtinguish = true

        case _ =>
      }
    }

    constraints(
      Extinguishing <-> shouldExtinguish &&
      OutOfWater <-> noWater &&
      ToRefill <-> headingToRefill &&
      Ready <-> !noWater && !shouldExtinguish
    )

    // action
    def extinguish = ???
    var noWater: Boolean = ???
    var shouldExtinguish: Boolean = ???
    var headingToRefill: Boolean = ???

    actions {
      states.selectedMembers.foreach {
        case OutOfWater =>
          agent.sendSpeak(time, Constants.TO_STATION, Message.encode(new NoWater()))

        case ToRefill =>
          // TODO - move to nearest water source

        case _ =>
      }
    }
  }

  class FireStation(entityID: EntityID, _position: Position) extends Component {
    //val id = entityID
    //name(s"FireStation $entityID")
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
      brigades.all(_.fireToExtinguish == fire)
    )

    // utility function not needed - TODO - can be ommited?
    // utility {
    // }

    actions {
      selectBrigadeForExtinguishingIfNeeded
    }

    def selectBrigadeForExtinguishingIfNeeded: Unit = {
      if (selectBrigadeForExtinguishing && !brigadesAvailable.isEmpty) {
        // TODO - send message to brigade

        currentlyExtinguishing = brigadesAvailable.head
        brigadesAvailable.drop(1)

        selectBrigadeForExtinguishing = false
      }
    }
  }

}