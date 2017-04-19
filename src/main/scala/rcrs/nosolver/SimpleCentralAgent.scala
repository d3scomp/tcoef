package rcrs.nosolver

import java.util.{Collection, EnumSet}

import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.events.EventHandler
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import rcrs.comm.{Constants, FireBrigadeToInitiator, InitiatorToFireBrigade, Message}
import rcrs.scenario.ProtectScenario
import rcrs.scenario.ScenarioUtils._
import rcrs.traits.map2d.{BuildingStatus, RCRSMapStatic, RCRSNodeStatus}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.components.StandardAgent
import rescuecore2.standard.entities.{FireBrigade => RescueFireBrigade, _}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}
import tcof.traits.map2d.{Map2D, Node, Position}

import scala.collection.mutable



private[nosolver] trait RCRSMapAdapterTrait extends RCRSTrait {
  this: Map2DTrait[RCRSNodeStatus] =>

  import scala.collection.JavaConverters._

  val rcrsMap: RCRSMap2D = new RCRSMap2D

  override def init(): Unit = {
    super.init()

    rcrsMap.populate()
  }

  class RCRSMap2D {
    private val areaIdToNode = mutable.Map.empty[EntityID, Node[RCRSNodeStatus]]
    private val nodeToArea = mutable.Map.empty[Node[RCRSNodeStatus], Area]

    def toNode(areaId: EntityID): Node[RCRSNodeStatus] = areaIdToNode(areaId)

    def toArea(node: Node[RCRSNodeStatus]): Area = nodeToArea(node)

    def toAreaID(path: List[Node[RCRSNodeStatus]]): List[EntityID] = path.map(toArea(_).getID)

    def currentNode = areaIdToNode(agent.currentAreaId)

    val lineOfSight = mutable.Map.empty[Node[RCRSNodeStatus], Set[Node[RCRSNodeStatus]]]

    val nodeStatus = mutable.Map.empty[Node[RCRSNodeStatus], RCRSNodeStatus]

    val closeAreaIDs = RCRSMapStatic.closeAreaIDs

    def getWalkedPath(origin: Node[RCRSNodeStatus], path: List[Node[RCRSNodeStatus]], history: List[Position]): List[Node[RCRSNodeStatus]] = {
      val histAreas = history.map(pos =>
        agent.model.getObjectsInRange(pos.x.toInt, pos.y.toInt, 0).asScala
          .collectFirst { case area: Area if area.getShape.contains(pos.x, pos.y) => area }.get
      )

      // Logger.info(s"Computing walked segment:\n  origin=${ toArea(origin)}\n  path=${path.map(toArea).toString}\n  history=${ histAreas }")

      val histIterator = histAreas.iterator

      var pathArea = toArea(origin)
      var remainingPath = path
      var remainingAreas = path.map(toArea)
      val walkedPath = mutable.ListBuffer.empty[Node[RCRSNodeStatus]]

      while (histIterator.hasNext) {
        val histArea = histIterator.next

        if (histArea == pathArea || remainingAreas.contains(histArea)) {
          while (histArea != pathArea) {
            val pathNode = remainingPath.head
            remainingPath = remainingPath.tail
            remainingAreas = remainingAreas.tail
            pathArea = toArea(pathNode)
            walkedPath += pathNode
          }
        }

        // If the condition above does not hold, we are slightly off the track. Either this gets corrected later in
        // the histAreas or it gets corrected in the next walk
      }

      walkedPath.toList
    }

    def populate(): Unit = {
      RCRSMapStatic.initialize(agent.config, agent.model)

      val model = agent.model.asScala

      for (entity <- model) {
        entity match {
          case area: Area =>
            val node = map.addNode(Position(area.getX, area.getY))
            areaIdToNode += (area.getID -> node)
            nodeToArea += (node -> area)
          case _ =>
        }
      }

      for (entity <- model) {
        entity match {
          case area: Area =>
            val areaNode = areaIdToNode(area.getID)
            val areaPosition = areaNode.center

            for (neighborId <- area.getNeighbours asScala) {
              val neighbor = agent.model.getEntity(neighborId)
              neighbor match {
                case _: Area =>
                  val neighborNode = areaIdToNode(neighborId)
                  val neighborPosition = neighborNode.center
                  map.addDirectedEdge(areaNode, neighborNode, areaPosition.distanceTo(neighborPosition))

                case _ =>
              }
            }

          case _ =>
        }
      }

      lineOfSight ++= RCRSMapStatic.lineOfSight.map { case (area, areasInSight) => (toNode(area) -> areasInSight.map(toNode)) }
    }

    def rcrsAreaExploration(origin: Node[RCRSNodeStatus], toExplore: Set[Node[RCRSNodeStatus]]): map.AreaExploration = map.areaExploration(origin, toExplore, lineOfSight)
  }


  implicit def map2dToRcrsMap2D(value: Map2D[RCRSNodeStatus]) = rcrsMap
}


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

      brigades.map { b =>
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
    val filteredGroupsWithTargets = groupsWithTargets.filter(grp => grp.forall {
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
      val groupWithMaxUtility = filteredGroupsWithTargets.maxBy {
        _.map(trav).sum
      }
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

            val nodeStatus = statusMap.map {
              case (idx, status) =>
                map.toNode(new EntityID(idx)) -> status
            }
            map.nodeStatus ++= nodeStatus

            // update also agent's world model
            statusMap.foreach {
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
      .collect { case building: Building if building.isOnFire => map.toNode(building.getID) }
  }

  override def agent: ScalaAgent = this
}

//
// code from traits
//


private[nosolver] abstract class ScalaAgent {

  sagent =>

  import scala.collection.JavaConverters._
  import scala.collection.mutable.ListBuffer

  type AgentEntityType <: StandardEntity

  protected def postConnect(): Unit = {
  }

  protected def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
  }


  protected def getRequestedEntityURNs: List[StandardEntityURN]

  def currentAreaId = me match {
    case area: Area => area.getID
    case human: Human => human.getPosition
    case _ => null
  }

  def config = rcrsAgent.delegateConfig

  def model = rcrsAgent.delegateModel

  def me = rcrsAgent.delegateMe

  def sendSubscribe(time: Int, channels: Int*) = rcrsAgent.delegateSendSubscribe(time, channels: _*)

  def sendSpeak(time: Int, channel: Int, data: Array[Byte]) = rcrsAgent.delegateSendSpeak(time, channel, data)

  var ignoreAgentCommandsUntil: Int = _

  class Agent extends StandardAgent[AgentEntityType] {
    override def getRequestedEntityURNsEnum: EnumSet[StandardEntityURN] = EnumSet.copyOf(sagent.getRequestedEntityURNs.asJavaCollection)

    override def think(time: Int, changes: ChangeSet, heard: Collection[Command]): Unit = sagent.think(time, changes, heard.asScala.toList)

    override protected def postConnect() {
      super.postConnect()

      ignoreAgentCommandsUntil = config.getIntValue(kernel.KernelConstants.IGNORE_AGENT_COMMANDS_KEY)

      sagent.postConnect()
    }

    def delegateConfig = config

    def delegateModel = model

    def delegateMe = me

    def delegateSendMove(time: Int, path: List[EntityID], destX: Int, destY: Int) = sendMove(time, ListBuffer(path: _*).asJava, destX, destY)

    def delegateSendSubscribe(time: Int, channels: Int*) = sendSubscribe(time, channels: _*)

    def delegateSendSpeak(time: Int, channel: Int, data: Array[Byte]) = sendSpeak(time, channel, data)
  }

  val rcrsAgent = new Agent
}

private[nosolver] trait Map2DTrait[NodeStatusType] extends Trait {

  val map: Map2D[NodeStatusType] = new Map2D[NodeStatusType]

  override def init(): Unit = {
    super.init()
  }
}

private[nosolver] trait StateSpaceTrait {

  def statespace(fun: Double => Double, t0: Double, y0: Double): StateSpaceModel1 = {
    val model = new FirstOrderDifferentialEquations {
      override def getDimension = 1

      override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
        yDot(0) = fun(y(0))
      }
    }

    new StateSpaceModel1(model, t0, y0)
  }
}

private[nosolver] trait RCRSTrait extends Trait {
  def agent: ScalaAgent
}

private[nosolver] abstract class StateSpaceModel private[nosolver](val model: FirstOrderDifferentialEquations, val t0: Double, val y0: Array[Double]) {

  class StopEvent(val targetValue: Double, val idx: Int) extends EventHandler {
    override def init(t0: Double, y0: Array[Double], t: Double) = {}

    override def eventOccurred(t: Double, y: Array[Double], increasing: Boolean) = EventHandler.Action.STOP

    override def g(t: Double, y: Array[Double]) = y(idx) - targetValue

    override def resetState(t: Double, y: Array[Double]) = {}
  }

  protected def value(time: Double): Array[Double] = {
    if (time == t0)
      y0
    else {
      val dp853 = new DormandPrince853Integrator(1.0e-8, 100.0, 1.0e-10, 1.0e-10)

      var y = new Array[Double](y0.size)
      dp853.integrate(model, t0, y0, time, y)
      y
    }
  }
}

private[nosolver] class StateSpaceModel1 private[nosolver](model: FirstOrderDifferentialEquations, t0: Double, y0: Double) extends StateSpaceModel(model, t0, Array(y0)) {
  def valueAt(time: Double): Double = value(time)(0)
}

private[nosolver] trait Trait {
  def init(): Unit = {
  }

}
