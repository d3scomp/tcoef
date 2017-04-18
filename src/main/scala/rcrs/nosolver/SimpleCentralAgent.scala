package rcrs.nosolver

import rcrs.ScalaAgent
import rcrs.comm._
import rcrs.scenario.ProtectScenario
import rcrs.scenario.ScenarioUtils._
import rcrs.traits.map2d.{BuildingStatus, RCRSMapAdapterTrait, RCRSNodeStatus}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade => RescueFireBrigade, _}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}
import tcof.traits.map2d.{Map2DTrait, Node, Position}
import tcof.traits.statespace.StateSpaceTrait


// In code with solver, functionality of this class is contained in:
// - CentralAgent
// - ProtectionScenario.Firestation,
// - ProtectionScenario.ProtectionTeam, ProtectionScenario.ExtinguishTeam, ProtectionScenario.FireCoordination (description of ensembles)
//
class SimpleCentralAgent extends ScalaAgent with Map2DTrait[RCRSNodeStatus] with RCRSMapAdapterTrait /* added inherit map methods */ with StateSpaceTrait {
  override type AgentEntityType = Building

  var fireBrigades: Map[EntityID, SimpleFireBrigade] = _

  override protected def postConnect() {
    Logger.info(s"Central agent connected")

    fireBrigades = createSimpleFireBrigades

    super.postConnect()
    this.init()
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"CentralAgent: Think called at time $time")
    super.think(time, changes, heard)

    if (time == ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_STATION)
    }

    if (time >= ignoreAgentCommandsUntil) {

      // sensing - process heard messages
      processReceivedMessages(heard)

      val startTime = System.currentTimeMillis

      // ensembleResolution - compute solution with highest utility and assign it to SimpleFireBrigades
      val changedBrigades = resolveEnsembles(time)

      val endTime = System.currentTimeMillis
      val timeElapsed = endTime - startTime
      Logger.info(s">>>> Time taken: ${timeElapsed} ms")

      // coordination - send message to changed brigades
      coordination(changedBrigades, time)
    }
  }

  private def resolveEnsembles(time: Int): List[SimpleFireBrigade] = {
    val solution = resolveProtectionTeams(time)
    for ((brigades, fire) <- solution) {
      for (brigade <- brigades) {
        brigade.assignedFireLocation = Some(fire)
        import ProtectScenario.FireBrigadeStatic.MirrorState._

        brigade.brigadeState = ProtectingMirror
      }
    }

    // return all (changed) brigades in current solution
    solution.flatMap(_._1)
  }

  private def resolveProtectionTeams(time: Int) = {
    def trav(tuple: (List[SimpleFireBrigade], Node[RCRSNodeStatus])): Double = {
      val (brigades, tgt) = tuple
      val routesToFireLocation = map.shortestPath.to(tgt)

      brigades.map{b =>
        val brigadeLocation = mapPosition(b)
        routesToFireLocation.costFrom(brigadeLocation).get
      }.sum
    }

    import ProtectScenario.FireBrigadeStatic.MirrorState._

    val availableBrigades = fireBrigades.values.filter(brigade => brigade.brigadeState == IdleMirror || brigade.brigadeState == ProtectingMirror)
    val fires = findBuildingsOnFire(map.nodes).toList

    val groups = GroupGenerator.generate(2, 3)(availableBrigades)
    val groupsWithTargets = GroupGenerator.zipWithPermutations(groups)(fires)

    // filter groupsWithTargets by firePredictor, TODO - NPE
    val filteredGroupsWithTargets = groupsWithTargets.filter(grp => grp.forall{
      case (part, fire) =>
        val routesToFireLocation: map.ShortestPathTo = map.shortestPath.to(fire)
        part.forall(brigade => routesToFireLocation.costFrom(mapPosition(brigade)) match {
          case None => false
          case Some(travelTime) =>
            val fireLocationNode = fire.asInstanceOf[Node[BuildingStatus]]
            val fierynessVal = if (fireLocationNode.status != null) fireLocationNode.status.fieryness else 0
            val firePredictor = statespace(burnModel(fireLocationNode), time, fierynessValue(fierynessVal))
            firePredictor.valueAt(travelTime) < 0.9
        })
    })

    // compute utility + select max
    // TODO - break after given number of steps?
    // NOTE - more effective would be to compute maximum
    // together with filtering (prev. step)
    if (filteredGroupsWithTargets.isEmpty) {
      Nil
    } else {
      val groupWithMaxUtility = filteredGroupsWithTargets.maxBy{_.map(trav).sum}
      groupWithMaxUtility
    }
  }

  // TODO - copy paste
  private def mapPosition(fireBrigade: SimpleFireBrigade): Node[RCRSNodeStatus] = {
    val human: Human = model.getEntity(fireBrigade.entityID).asInstanceOf[Human]
    map.toNode(human.getPosition)
  }

  private def coordination(changedBrigades: List[SimpleFireBrigade], time: Int) = {
    for (brigade <- changedBrigades) {
      val buildingEntityID = brigade.assignedFireLocation.map(map.toArea(_).getID)
      val message = InitiatorToFireBrigade(brigade.entityID, brigade.brigadeState, buildingEntityID)
      agent.sendSpeak(time, Constants.TO_AGENTS, Message.encode(message))
    }
  }

  // TODO - copy paste
  private def processReceivedMessages(heard: List[Command]): Unit = {
    heard.foreach {
      case speak: AKSpeak => {
        val msg = Message.decode(speak.getContent)

        msg match {
          case FireBrigadeToInitiator(mirrorState, position, currentAreaId, statusMap) =>
            // update state
            val brigade = fireBrigades(speak.getAgentID)
            brigade.brigadePosition = position
            brigade.brigadeState = mirrorState

            // update also agent's world model
            agent.model.getEntity(speak.getAgentID).asInstanceOf[RescueFireBrigade].setPosition(currentAreaId, position.x.toInt, position.y.toInt)

            val nodeStatus = statusMap.map{
              case (idx, status) =>
                map.toNode(new EntityID(idx)) -> status
            }
            map.nodeStatus ++= nodeStatus

            // update also agent's world model
            statusMap.foreach{
              case (id, status) =>
                val building = agent.model.getEntity(new EntityID(id)).asInstanceOf[Building]
                val buildingStatus = status.asInstanceOf[BuildingStatus]
                building.setTemperature(buildingStatus.temperature)
                building.setBrokenness(buildingStatus.brokenness)
                building.setFieryness(buildingStatus.fieryness)
            }

          case _ =>
        }
      }
      case _ =>
    }
  }

  /**
    * Creates FireBrigade components and assigns them position from configuration.
    */
  private def createSimpleFireBrigades: Map[EntityID, SimpleFireBrigade] = {
    findEntities[RescueFireBrigade](model, StandardEntityURN.FIRE_BRIGADE).map { fb =>
      val model = rcrsAgent.delegateModel
      val location = fb.getLocation(model)
      fb.getID -> new SimpleFireBrigade(fb.getID, Position(location.first.toDouble, location.second.toDouble))
    }.toMap
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(
    StandardEntityURN.FIRE_STATION
  )

  /** Replacement for FireBrigadeComponent */
  class SimpleFireBrigade(val entityID: EntityID, var brigadePosition: Position) {
    import ProtectScenario.FireBrigadeStatic.MirrorState._

    var brigadeState = IdleMirror
    var assignedFireLocation: Option[Node[RCRSNodeStatus]] = None
  }

  // TODO - copy + paste, but hard to move to ScenarioUtils (toArea method)
  private def findBuildingsOnFire(nodes: Seq[Node[RCRSNodeStatus]]): Seq[Node[RCRSNodeStatus]] = {
    nodes.map(map.toArea)
      .collect{ case building: Building if building.isOnFire => map.toNode(building.getID) }
  }

  override def agent: ScalaAgent = this
}

