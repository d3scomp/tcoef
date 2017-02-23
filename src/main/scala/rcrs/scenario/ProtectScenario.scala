package rcrs.scenario

import rcrs.comm._
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus
import rcrs.{FireBrigadeAgent, ScalaAgent}
import rescuecore2.standard.entities.{FireBrigade => RescueFireBrigade, StandardEntityURN, Building, Refuge}
import rescuecore2.worldmodel.EntityID
import tcof.InitStages.InitStages
import tcof._
import tcof.traits.map2d.{Map2DTrait, Node, Position}

object ProtectScenario {
  object FireBrigadeStatic {

    /** Representation of the component's state, transferred between component and ensemble
      * and used in computations on the initiator of the ensemble. */
    object MirrorState extends Enumeration {
      type MirrorState = Value
      val ProtectingMirror, RefillingMirror, IdleMirror = Value
    }

  }
}

import ProtectScenario.FireBrigadeStatic.MirrorState._

class ProtectScenario(scalaAgent: ScalaAgent) extends Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] {
  this.agent = scalaAgent

  class FireBrigade(val entityID: EntityID, var brigadePosition: Position) extends Component {

    // states are used only for resolution in component, not propagated to ensemble
    private var Protecting: State = _
    private var Refilling: State = _
    private var Idle: State = _
    private var Operational: State = _

    override def _init(stage: InitStages, config: Config): Unit = {
      super._init(stage, config)

      stage match {
        case InitStages.CreateCustomStates =>
          Protecting = State
          Refilling = State
          Idle = State
          Operational = StateOr(Protecting, Refilling, Idle)
        case _ =>
      }
    }

    // information transferred between initiator and component - start

    // fb -> initiator - fb changes state to Refilling when runs out of water
    // initiator -> fb - ensemble changes state from Idle to Protecting
    var brigadeState = IdleMirror

    // fb -> initiator - current fb position
    // brigadePosition - obtained in constructor (initial value assigned from)

    // fb -> initiator - fire is extinguished or when refilling (sets to None)
    // initiator -> fb - assigns fire
    var assignedFireLocation: Option[EntityID] = None

    // information transferred between initiator and component - end


    preActions {
      brigadePosition = agent.getPosition
      processReceivedMessages()

//      println(s"${Thread.currentThread}: brigade ${entityID} (preActions)\tstate: ${brigadeState} assignedFireLocation: ${assignedFireLocation}")
    }

    constraints {
      Operational &&
        (Protecting -> (brigadeState == ProtectingMirror)) &&
        (Idle -> (brigadeState == IdleMirror)) &&
        (Refilling <-> (refillingAtRefuge || tankEmpty))
    }

    actions {
//      println(s"${Thread.currentThread}: brigade ${entityID} (actions)\t Protecting=${states.selectedMembers.exists(_ == Protecting)} Refilling=${states.selectedMembers.exists(_ == Refilling)} Idle=${states.selectedMembers.exists(_ == Idle)}")

      syncFields()
      sendMessages()
      performAction()
    }

    private def syncFields(): Unit = {
      brigadeState = if (states.selectedMembers.exists(_ == Refilling)) {
        RefillingMirror
      } else if (states.selectedMembers.exists(_ == Protecting)) {
        ProtectingMirror
      } else {
        IdleMirror
      }

      if (brigadeState != ProtectingMirror) {
        assignedFireLocation = None
      }
    }

    private def sendMessages(): Unit = {
      val message = FireBrigadeToInitiator(brigadeState, brigadePosition)
      agent.sendSpeak(time, Constants.TO_STATION, Message.encode(message))
    }

    private def performAction(): Unit = {
      brigadeState match {
        case RefillingMirror if !refillingAtRefuge =>
          moveTo(nearestRefuge)

        case ProtectingMirror =>
          if (inExtinguishingDistanceFromFire) {
            extinguish()
          } else {
            // TODO - move near fire
            moveTo(assignedBuildingOnFire)
          }

        case _ =>
          rest()
      }
    }

    private def nearestRefuge: Node[RCRSNodeStatus] = {
      // TODO - dummy implementation
      import collection.JavaConverters._
      val refuge = agent.model.getEntitiesOfType(StandardEntityURN.REFUGE).asScala.head.asInstanceOf[Refuge]
      map.toNode(refuge.getID)
    }

    private def assignedBuildingOnFire: Node[RCRSNodeStatus] = {
      map.toNode(assignedFireLocation.get)
    }

    private def inExtinguishingDistanceFromFire: Boolean = {
      val maxDistance = agent.asInstanceOf[FireBrigadeAgent].maxDistance
      // TODO
      false
    }

    private def moveTo(node: Node[RCRSNodeStatus]) = {
      // TODO - not very effective - recomputes shortest path in every step
      val currentNode = map.toNode(agent.currentAreaId)
      val path = map.shortestPath.from(currentNode).pathTo(node)
      val entityIdPath = map.toAreaID(path.get)
      agent.sendMove(time, entityIdPath)
    }

    private def rest(): Unit = {
      agent.sendRest(time)
    }

    def extinguish(): Unit = {
      agent.sendExtinguish(time, assignedFireLocation.get, agent.asInstanceOf[FireBrigadeAgent].maxPower)
    }

    private def processReceivedMessages(): Unit = {
      sensing.messages.foreach{
        case (InitiatorToFireBrigade(receiverId, mirrorState, fireLocation), _) if receiverId == agent.getID =>
          brigadeState = mirrorState
          assignedFireLocation = fireLocation

        case _ =>
      }
    }

    private def waterLevel: Int = agent.me.asInstanceOf[RescueFireBrigade].getWater
    private def refillingAtRefuge: Boolean = agent.location.isInstanceOf[Refuge] && waterLevel < agent.asInstanceOf[FireBrigadeAgent].maxWater
    private def tankEmpty: Boolean = waterLevel == 0
  }


  class FireStation(val entityID: EntityID) extends Component {
    val fireCoordination = root(new FireCoordination(this))

    preActions {
      processReceivedMessages()
    }

    actions {
      fireCoordination.init()

      // TODO - should solve and commit be called here? IMHO yes as sendSpeak sends updated attribute values
      while (fireCoordination.solve()) {
        //println(fireCoordination.instance.toStringWithUtility)
      }

      fireCoordination.commit()


      for (protectionTeam <- fireCoordination.instance.protectionTeams.selectedMembers)
        for (brigade <- protectionTeam.brigades.selectedMembers) {
          brigade.brigadeState = ProtectingMirror
          // TODO - switch component to Idle when fire is extinguished
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

    membership {
      brigades.all(brigade => (brigade.brigadeState == IdleMirror)
        || (brigade.brigadeState == ProtectingMirror) && sameLocations(brigade.assignedFireLocation)) &&
        brigades.cardinality >= 2 && brigades.cardinality <= 3
    }

    utility {
      // TODO - this utility function will always form team of 2 fire brigades
      // The utility function should probably prefer 3 brigades, something like this:
      // brigades.sum(proximityToFire) + (brigades.cardinality * 100)
      // - "*" defined in Integer would be needed
      brigades.sum(proximityToFire)
    }

    actions {
      for (brigade <- brigades.selectedMembers) {
        brigade.assignedFireLocation = Some(fireLocation)
        assignRoleAndBuildingsToProtect(brigade)
      }
    }

    private def proximityToFire(brigade: FireBrigade): Int = {
      val firePosition = map.toNode(fireLocation).center
      // shifted to avoid 0 as max for empty ensemble
      100 - (brigade.brigadePosition.distanceTo(firePosition) / 10000).round.toInt
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

    private val buildingsOnFire = findBuildingsOnFire(map.nodes)

    // assigns 2-3 brigades to each building - there can be many brigades unassigned
    val extinguishTeams = ensembles(buildingsOnFire.map(new ExtinguishTeam(coordinator, _)))
    val protectionTeams = ensembles(buildingsOnFire.map(new ProtectionTeam(coordinator, _)))

    membership {
      (extinguishTeams.map(_.brigades) ++ protectionTeams.map(_.brigades)).allDisjoint
    }

    private def findBuildingsOnFire(nodes: Seq[Node[RCRSNodeStatus]]): Seq[EntityID] = {
//      nodes.map(map.toArea)
//        .collect{ case building: Building if building.isOnFire => building }
//        .map(_.getID)

      // TODO - remove, mock
      nodes.map(map.toArea)
          .collect{ case building: Building => building }
          .take(2)
          .map(_.getID)
    }
  }
}