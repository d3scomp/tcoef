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
    var brigadePosition: Position = getInitPosition(entityID)

    // fb -> initiator - fire is extinguished or when refilling (sets to null)
    // initiator -> fb - assigns fire
    var assignedFireLocation: Option[EntityID] = None

    // information transferred between initiator and component - end


    // states are used only for resolution in component, not propagated to ensemble
    val Idle = State
    val Protecting = State
    val Refilling = State
    val Operational = StateOr(Idle, Protecting, Refilling) // to prevent brigade to be in multiple states at the same time

    preActions(
      processReceivedMessages()
    )

    constraints(
      Protecting <-> (assignedFireLocation != null && brigadeState == ProtectingMirror) &&
      Refilling -> (refillingAtRefuge || tankEmpty)
    )

    def processReceivedMessages(): Unit = {
      sensing.messages.foreach{
        case (InitiatorToFireBrigade(receiverId, mirrorState, fireLocation), _) if receiverId == agent.getID =>
          brigadeState = mirrorState
          assignedFireLocation = fireLocation

        case _ =>
      }
    }

    def getInitPosition(entityID: EntityID): Position = {
      val model = agent.model
      val location = model.getEntity(entityID).getLocation(model)
      Position(location.first.toInt, location.second.toInt)
    }

    def getInitWaterLevel(entityID: EntityID): Int = {
      val brigade = agent.model.getEntity(entityID).asInstanceOf[RescueFireBrigade]
      brigade.getWater
    }

    def waterLevel: Int = agent.me.asInstanceOf[RescueFireBrigade].getWater
    def refillingAtRefuge: Boolean = agent.location.isInstanceOf[Refuge] && waterLevel < agent.asInstanceOf[FireBrigadeAgent].maxWater
    def tankEmpty: Boolean = waterLevel == 0
  }

  class FireStation(val entityID: EntityID) extends Component {
    val fireCoordination = new FireCoordination(this)
    val fireCoordinationRoot = root(fireCoordination)

    preActions(
      processReceivedMessages()
    )

    actions {
      fireCoordinationRoot.init()

      // TODO - should solve and commit be called here? IMHO yes as sendSpeak sends updated attribute values

      for (protectionTeam <- fireCoordination.protectionTeams.selectedMembers)
        for (brigade <- protectionTeam.brigades.selectedMembers) {
          // coordinator sends attribute values to selected components:
          // - entityID of fire
          // - role of the brigade at fire
          // - state of the component (e.g. Protecting)
          // TODO - how is component switched back to Idle? Is that automatic when component runs
          // out of water or when fire is extinguished ?
//          val message = InitiatorToFireBrigade(brigade.entityID, brigade.brigadeState.id, brigade.assignedFireLocation, )
//          agent.sendSpeak(time, Constants.TO_AGENTS, Message.encode(??? /*TODO*/ ))
        }
      // ...
    }

    def processReceivedMessages(): Unit = {
      sensing.messages.foreach{
        case (FireBrigadeToInitiator(mirrorState, position), message) =>
          updateInitiatorKnowledge(message.getAgentID, mirrorState, position)

        case _ =>
      }
    }

    def updateInitiatorKnowledge(id: EntityID, mirrorState: MirrorState, position: Position): Unit = {
      val brigade = components.collect{ case x: FireBrigade => x}.find(_.entityID == id).get
      brigade.brigadeState = mirrorState
      brigade.brigadePosition = position
    }
  }

  class ProtectionTeam(coordinator: FireStation, fireLocation: EntityID) extends Ensemble {

    val brigades = role("brigades",components.select[FireBrigade])

    membership(
      brigades.all(brigade => (brigade.brigadeState == IdleMirror)
        || (brigade.brigadeState == ProtectingMirror) && brigade.assignedFireLocation == fireLocation) &&
              brigades.cardinality >= 2 && brigades.cardinality <= 3
    )

    actions {
      for (brigade <- brigades.selectedMembers) {
        brigade.assignedFireLocation = Some(fireLocation)
        assignRoleAndBuildingsToProtect(brigade)
      }
    }

    def assignRoleAndBuildingsToProtect(brigade: FireBrigade) = ???
  }

  class ExtinguishTeam(coordinator: FireStation, fireLocation: EntityID) extends Ensemble {
    // ...
    val brigades = role("brigades",components.select[FireBrigade])
  }

  class FireCoordination(coordinator: FireStation) extends RootEnsemble /* TODO - will extend just Ensamble */ {

    val buildingsOnFire: Seq[EntityID] = findBuildingsOnFire(map.nodes)

    // assigns 2-3 brigades to each building - there can be many brigades unassigned
    val extinguishTeams = ensembles(buildingsOnFire.map(new ExtinguishTeam(coordinator, _)))
    val protectionTeams = ensembles(buildingsOnFire.map(new ProtectionTeam(coordinator, _)))

    membership(
      (extinguishTeams.map(_.brigades) ++ protectionTeams.map(_.brigades)).allDisjoint
    )

    def findBuildingsOnFire(nodes: Seq[Node[RCRSNodeStatus]]): Seq[EntityID] = {
      nodes.map{map.toArea(_)}
        .collect{ case building: Building if building.isOnFire => building }
        .map(_.getID)
    }
  }
}