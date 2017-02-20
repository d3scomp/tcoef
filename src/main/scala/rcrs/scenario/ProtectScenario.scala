package rcrs.scenario

import rcrs.comm._
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus
import rcrs.{FireBrigadeAgent, ScalaAgent}
import rescuecore2.standard.entities.{Building, FireBrigade => RescueFireBrigade, Refuge}
import rescuecore2.worldmodel.EntityID
import tcof._
import tcof.traits.map2d.{Map2DTrait, Node, Position}

object ProtectScenario {
  object FireBrigadeStatic {

    /** Representation of the component's state, transferred between component and ensemble
      * and used in computations on the initiator of the ensemble. */
    object MirrorState extends Enumeration {
      type MirrorState = Value
      val IdleMirror, ProtectingMirror, RefillingMirror = Value
    }

  }
}

import ProtectScenario.FireBrigadeStatic.MirrorState._

class ProtectScenario(scalaAgent: ScalaAgent) extends Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] {
  this.agent = scalaAgent

  class FireBrigade(val entityID: EntityID) extends Component {
    // information transferred between initiator and component - start

    // fb -> initiator - fb changes state to Refilling when runs out of water
    // initiator -> fb - ensemble changes state from Idle to Protecting
    var brigadeState = IdleMirror

    // fb -> initiator - current fb position
    // initialized and updated in preactions (cannot be initialized before rcrs model creation)
    var brigadePosition: Position = _

    // fb -> initiator - fire is extinguished or when refilling (sets to None)
    // initiator -> fb - assigns fire
    var assignedFireLocation: Option[EntityID] = None

    // information transferred between initiator and component - end


    // states are used only for resolution in component, not propagated to ensemble
    private val Idle, Protecting, Refilling = State
    private val Operational = StateOr(Idle, Protecting, Refilling) // to prevent brigade to be in multiple states at the same time, TODO - discuss whether use Operational

    preActions {
      brigadePosition = getPosition
      processReceivedMessages()
    }

    constraints(
      Protecting -> (assignedFireLocation.isDefined && brigadeState == ProtectingMirror) &&
      Refilling <-> (refillingAtRefuge || tankEmpty) &&
      Idle -> (!assignedFireLocation.isDefined)
    )

    actions {
      syncMirrorBrigadeState()
      val message = FireBrigadeToInitiator(brigadeState, brigadePosition)
      agent.sendSpeak(time, Constants.TO_STATION, Message.encode(message))
    }

    private def syncMirrorBrigadeState(): Unit = {
      brigadeState = if (states.selectedMembers.exists(_ == Refilling)) {
         RefillingMirror
      } else if (states.selectedMembers.exists(_ == Protecting)) {
        ProtectingMirror
      } else {
        IdleMirror
      }
    }

    private def processReceivedMessages(): Unit = {
      sensing.messages.foreach{
        case (InitiatorToFireBrigade(receiverId, mirrorState, fireLocation), _) if receiverId == agent.getID =>
          brigadeState = mirrorState
          assignedFireLocation = fireLocation

        case _ =>
      }
    }

    private def getPosition: Position = {
      val model = agent.model
      val location = model.getEntity(entityID).getLocation(model)
      Position(location.first.toDouble, location.second.toDouble)
    }

    private def getInitWaterLevel(entityID: EntityID): Int = {
      val brigade = agent.model.getEntity(entityID).asInstanceOf[RescueFireBrigade]
      brigade.getWater
    }

    private def waterLevel: Int = agent.me.asInstanceOf[RescueFireBrigade].getWater
    private def refillingAtRefuge: Boolean = agent.location.isInstanceOf[Refuge] && waterLevel < agent.asInstanceOf[FireBrigadeAgent].maxWater
    private def tankEmpty: Boolean = waterLevel == 0
  }


  class FireStation(val entityID: EntityID) extends Component {
    private val fireCoordination = new FireCoordination(this)
    private val fireCoordinationRoot = root(fireCoordination)

    preActions(
      processReceivedMessages()
    )

    actions {
      fireCoordinationRoot.init()

      // TODO - should solve and commit be called here? IMHO yes as sendSpeak sends updated attribute values

      for (protectionTeam <- fireCoordination.protectionTeams.selectedMembers)
        for (brigade <- protectionTeam.brigades.selectedMembers) {
          // TODO - how is component switched back to Idle? Is that automatic when component runs
          // out of water or when fire is extinguished ?
          val message = InitiatorToFireBrigade(brigade.entityID, brigade.brigadeState, brigade.assignedFireLocation)
          agent.sendSpeak(time, Constants.TO_AGENTS, Message.encode(message))
        }
      // ...
    }

    private def processReceivedMessages(): Unit = {
      sensing.messages.foreach{
        case (FireBrigadeToInitiator(mirrorState, position), message) =>
          updateInitiatorKnowledge(message.getAgentID, mirrorState, position)

        case _ =>
      }
    }

    private def updateInitiatorKnowledge(id: EntityID, mirrorState: MirrorState, position: Position): Unit = {
      val brigade = components.collect{ case x: FireBrigade => x}.find(_.entityID == id).get
      brigade.brigadeState = mirrorState
      brigade.brigadePosition = position
      // TODO - update position in rcrs model?
    }
  }

  class ProtectionTeam(coordinator: FireStation, fireLocation: EntityID) extends Ensemble {

    val brigades = role("brigades",components.select[FireBrigade])

    membership(
      brigades.all(brigade => (brigade.brigadeState == IdleMirror)
        || (brigade.brigadeState == ProtectingMirror) && sameLocations(brigade.assignedFireLocation)) &&
              brigades.cardinality >= 2 && brigades.cardinality <= 3
    )

    actions {
      for (brigade <- brigades.selectedMembers) {
        brigade.assignedFireLocation = Some(fireLocation)
        assignRoleAndBuildingsToProtect(brigade)
      }
    }

    private def sameLocations(optionalLocation: Option[EntityID]): Boolean = {
      optionalLocation match {
        case Some(location) => fireLocation == location
        case _ => false
      }
    }

    private def assignRoleAndBuildingsToProtect(brigade: FireBrigade) = {
      // TODO - "protection role" in Protect mode not defined
    }
  }

  class ExtinguishTeam(coordinator: FireStation, fireLocation: EntityID) extends Ensemble {
    // ...
    val brigades = role("brigades",components.select[FireBrigade])
  }

  class FireCoordination(coordinator: FireStation) extends RootEnsemble /* TODO - will extend just Ensamble */ {

    private var buildingsOnFire: Seq[EntityID] = _

    // assigns 2-3 brigades to each building - there can be many brigades unassigned
    var extinguishTeams: EnsembleGroup[ExtinguishTeam] = _
    var protectionTeams: EnsembleGroup[ProtectionTeam] = _

    preActions {
      // need to be recomputed in each step
      buildingsOnFire = findBuildingsOnFire(map.nodes)
      extinguishTeams = ensembles(buildingsOnFire.map(new ExtinguishTeam(coordinator, _)))
      protectionTeams = ensembles(buildingsOnFire.map(new ProtectionTeam(coordinator, _)))
    }

    membership(
      (extinguishTeams.map(_.brigades) ++ protectionTeams.map(_.brigades)).allDisjoint
    )

    private def findBuildingsOnFire(nodes: Seq[Node[RCRSNodeStatus]]): Seq[EntityID] = {
      nodes.map{map.toArea(_)}
        .collect{ case building: Building if building.isOnFire => building }
        .map(_.getID)
    }
  }
}