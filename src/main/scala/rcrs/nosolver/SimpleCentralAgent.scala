package rcrs.nosolver

import de.ummels.prioritymap.PriorityMap
import org.apache.commons.math3.analysis.interpolation.{LinearInterpolator, SplineInterpolator}
import org.apache.commons.math3.analysis.polynomials.PolynomialSplineFunction
import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.events.EventHandler
import org.apache.commons.math3.ode.nonstiff.DormandPrince853Integrator
import rcrs.scenario.MirrorState
import rcrs.scenario.MirrorState._
import rescuecore2.config.Config
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.components.StandardAgent
import rescuecore2.standard.entities.StandardEntityConstants.Fieryness
import rescuecore2.standard.entities.{FireBrigade => RescueFireBrigade, _}
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}
import scodec.Attempt.{Failure, Successful}
import scodec._
import scodec.bits.{BitVector, _}
import scodec.codecs.{constant, listOfN, uint, _}

import scala.collection.mutable
import scala.concurrent.Future

abstract class RCRSNodeStatus

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

    val availableBrigades = fireBrigades.values.filter(brigade => brigade.brigadeState == IdleMirror || brigade.brigadeState == ProtectingMirror)
    val fires = findBuildingsOnFire(map.nodes).toList

    val groups = GroupGenerator.generate(2, 3)(availableBrigades)
    val groupsWithTargets = GroupGenerator.zipWithPermutations(groups)(fires)

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

  def findEntities[T <: StandardEntity](model: StandardWorldModel, urn: StandardEntityURN): Iterable[T] = {
    import scala.collection.JavaConverters._
    model.getEntitiesOfType(urn).asScala
      .map{_.asInstanceOf[T]}
  }

  def travelTimeToUtility(routeTime: Option[Double]): Int = routeTime match {
    case None => 0
    case Some(time) => 100 - (time / 10000).round.toInt
  }

  def burnModel(node: Node[BuildingStatus]) = interpolate.linear(
    0.0 -> 0.0,
    0.5 -> 0.1,
    1.0 -> 0.0
  )

  def fierynessValue(fieryness: Int): Double = {
    import Fieryness._
    val f = Fieryness.values()(fieryness)
    f match {
      case UNBURNT | WATER_DAMAGE =>
        0.0
      case HEATING =>
        0.25
      case BURNING =>
        0.5
      case INFERNO =>
        0.75
      case _ =>
        1.0
    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(
    StandardEntityURN.FIRE_STATION
  )

  /** Replacement for FireBrigadeComponent */
  class SimpleFireBrigade(val entityID: EntityID, var brigadePosition: Position) {
    var brigadeState = IdleMirror
    var assignedFireLocation: Option[Node[RCRSNodeStatus]] = None
  }

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

  import java.util.{Collection, EnumSet}

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
  def location = rcrsAgent.delegateLocation
  def sendMove(time: Int, path: List[EntityID]) = rcrsAgent.delegateSendMove(time, path)
  def sendSubscribe(time: Int, channels: Int*) = rcrsAgent.delegateSendSubscribe(time, channels: _*)
  def sendRest(time: Int) = rcrsAgent.delegateSendRest(time)
  def sendSpeak(time: Int, channel: Int, data: Array[Byte]) = rcrsAgent.delegateSendSpeak(time, channel, data)
  def sendExtinguish(time: Int, target: EntityID, water: Int) = rcrsAgent.delegateSendExtinguish(time, target, water)

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
    def delegateLocation = location
    def delegateSendMove(time: Int, path: List[EntityID]) = sendMove(time, ListBuffer(path: _*).asJava)
    def delegateSendSubscribe(time: Int, channels: Int*) = sendSubscribe(time, channels: _*)
    def delegateSendRest(time: Int) = sendRest(time)
    def delegateSendSpeak(time: Int, channel: Int, data: Array[Byte]) = sendSpeak(time, channel, data)
    def delegateSendExtinguish(time: Int, target: EntityID, water: Int) = sendExtinguish(time, target, water)
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

object interpolate {
  private def interpolant(breakpoints: Seq[(Double, Double)], fun: PolynomialSplineFunction): Double => Double = {
    (x: Double) => {
      if (x > breakpoints.last._1) breakpoints.last._2
      else if (x < breakpoints.head._1) breakpoints.head._1
      else fun.value(x)
    }
  }

  def linear(breakpoints: (Double, Double)*): Double => Double =
    interpolant(breakpoints, new LinearInterpolator().interpolate(breakpoints.map(_._1).toArray, breakpoints.map(_._2).toArray))

  def spline(breakpoints: (Double, Double)*): Double => Double =
    interpolant(breakpoints, new SplineInterpolator().interpolate(breakpoints.map(_._1).toArray, breakpoints.map(_._2).toArray))
}

private[nosolver] trait Trait {
  def init(): Unit = {
  }

}

class Map2D[NodeStatusType] extends WithShortestPath[NodeStatusType] with WithAreaExploration[NodeStatusType] {
  private var _nodes = List.empty[Node[NodeStatusType]]
  private var _edges = List.empty[Edge[NodeStatusType]]

  def nodes: List[Node[NodeStatusType]] = _nodes

  def edges: List[Edge[NodeStatusType]] = _edges

  def addNode(center: Position): Node[NodeStatusType] = {
    val node = new Node(this, center)
    _nodes = _nodes :+ node
    node
  }

  def addDirectedEdge(from: Node[NodeStatusType], to: Node[NodeStatusType], cost: Double): Edge[NodeStatusType] = from._outNeighbors.get(to) match {
    case Some(edge) => edge

    case None =>
      val edge = new Edge(this, from, to, cost)
      _edges = _edges :+ edge
      from._outNeighbors = from._outNeighbors + (to -> edge)
      to._inNeighbors = to._inNeighbors + (from -> edge)
      edge
  }
}

class Node[NodeStatusType] private[nosolver](val map: Map2D[NodeStatusType], val center: Position) {
  private[nosolver] var _outNeighbors = Map.empty[Node[NodeStatusType], Edge[NodeStatusType]]
  private[nosolver] var _inNeighbors = Map.empty[Node[NodeStatusType], Edge[NodeStatusType]]

  def outNeighbors: Map[Node[NodeStatusType], Edge[NodeStatusType]] = _outNeighbors
  def inNeighbors: Map[Node[NodeStatusType], Edge[NodeStatusType]] = _inNeighbors

  var lastVisitTime = Int.MinValue

  var status: NodeStatusType = _

  override def toString() = s"Node(${center.x}, ${center.y})"
}

class Edge[NodeStatusType] private[nosolver](val map: Map2D[NodeStatusType], val from: Node[NodeStatusType], val to: Node[NodeStatusType], private var _cost: Double) {
  def cost = _cost
  def cost_=(value: Double) = {
    _cost = value
    map.shortestPath.invalidateCache()
  }
}

trait WithShortestPath[NodeStatusType] {
  this: Map2D[NodeStatusType] =>

  object shortestPath {
    private[WithShortestPath] val outCache = mutable.Map.empty[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]
    private[WithShortestPath] val inCache = mutable.Map.empty[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]
    private[WithShortestPath] var epoch = 0

    def invalidateCache(): Unit = {
      synchronized {
        outCache.clear()
        inCache.clear()
        epoch = epoch + 1
      }
    }

    def from(source: Node[NodeStatusType]): ShortestPathFrom = new ShortestPathFrom(source)
    def to(destination: Node[NodeStatusType]): ShortestPathTo = new ShortestPathTo(destination)
  }

  class ShortestPathFrom(source: Node[NodeStatusType]) extends ShortestPath(source) {
    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeStatusType]) = node.outNeighbors.values.map(edge => (edge.to, edge.cost))
    private[WithShortestPath] def cache = shortestPath.outCache

    def costTo(destination: Node[NodeStatusType]): Option[Double] = cost(destination)
    def pathTo(destination: Node[NodeStatusType]) = path(destination)
  }

  class ShortestPathTo(destination: Node[NodeStatusType]) extends ShortestPath(destination) {
    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeStatusType]) = node.inNeighbors.values.map(edge => (edge.from, edge.cost))
    private[WithShortestPath] def cache = shortestPath.inCache

    def costFrom(source: Node[NodeStatusType]): Option[Double] = cost(source)
    def pathFrom(source: Node[NodeStatusType]) = path(source)
  }

  abstract class ShortestPath(val origin: Node[NodeStatusType]) {
    val (nodesByDistance, distances, predecessors) = compute(origin)

    private[WithShortestPath] def getNeighborsWithCosts(node: Node[NodeStatusType]): Iterable[(Node[NodeStatusType], Double)]
    private[WithShortestPath] def cache: mutable.Map[Node[NodeStatusType], (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]])]

    // Adapted from https://github.com/ummels/dijkstra-in-scala/blob/master/src/main/scala/de/ummels/dijkstra/DijkstraPriority.scala
    // Original version - Copyright (c) 2015, Michael Ummels <michael@ummels.de>
    private def compute(origin: Node[NodeStatusType]): (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) = {
      def go(active: PriorityMap[Node[NodeStatusType], Double], nodesByDistance: List[Node[NodeStatusType]], distances: Map[Node[NodeStatusType], Double], predecessors: Map[Node[NodeStatusType], Node[NodeStatusType]]):
      (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) =
        if (active.isEmpty)
          (nodesByDistance.reverse.tail, distances, predecessors)
        else {
          val (node, cost) = active.head
          val neighbours = (for {
            (neigh, neighCost) <- getNeighborsWithCosts(node)
            if !distances.contains(neigh) && cost + neighCost < active.getOrElse(neigh, Double.MaxValue)
          } yield neigh -> (cost + neighCost)) toMap

          val preds = neighbours mapValues (_ => node)
          go(active.tail ++ neighbours, node :: nodesByDistance, distances + (node -> cost), predecessors ++ preds)
        }

      var result: (List[Node[NodeStatusType]], Map[Node[NodeStatusType], Double], Map[Node[NodeStatusType], Node[NodeStatusType]]) = null
      var epoch = 0

      synchronized {
        cache.get(origin) match {
          case Some(x) => result = x
          case None =>
        }

        epoch = shortestPath.epoch
      }

      if (result == null) {
        result = go(PriorityMap(origin -> 0), List.empty[Node[NodeStatusType]], Map.empty[Node[NodeStatusType], Double], Map.empty[Node[NodeStatusType], Node[NodeStatusType]])

        synchronized {
          if (shortestPath.epoch == epoch)
            cache += (origin -> result)
        }
      }

      result
    }

    private[WithShortestPath] def cost(target: Node[NodeStatusType]): Option[Double] = distances.get(target)

    private[WithShortestPath] def path(target: Node[NodeStatusType]) = {
      def go(current: Node[NodeStatusType], pathSoFar: List[Node[NodeStatusType]] = List()): List[Node[NodeStatusType]] = {
        predecessors.get(current) match {
          case None => pathSoFar
          case Some(node) => go(node, current :: pathSoFar)
        }
      }

      if (origin == target)
        Some(List())
      else if (predecessors.contains(target))
        Some(go(target))
      else
        None
    }
  }

}


trait WithAreaExploration[NodeStatusType] {
  this: Map2D[NodeStatusType] =>

  import scala.concurrent.ExecutionContext.Implicits.global

  def areaExploration(origin: Node[NodeStatusType], toExplore: Set[Node[NodeStatusType]], nodesInView: Node[NodeStatusType] => Iterable[Node[NodeStatusType]]): AreaExploration =
    new AreaExploration(origin, toExplore, nodesInView)

  /**
    * Approximates optimal path starting at origin that ensures that all nodes within rectangle [leftBottom, rightTop] are seen (i.e.
    * an agent is within sight distance.
    * @param nodesToExplore List of nodes to be explored
    * @param nodesInView A function that returns nodes that can be seen from a node
    */
  class AreaExploration(explorationOrigin: Node[NodeStatusType], val nodesToExplore: Set[Node[NodeStatusType]], val nodesInView: Node[NodeStatusType] => Iterable[Node[NodeStatusType]]) {
    private val exploreMaxCount = 3
    private val backtrackingMaxCount = 10000

    private var assumePathWithOrigin: List[Node[NodeStatusType]] = List(explorationOrigin)

    private var walkedPathWithOrigin: List[Node[NodeStatusType]] = List(explorationOrigin)

    private var currentTask: ComputationTask = null

    private class ComputationTask() {
      val localAssumePathWithOrigin = assumePathWithOrigin
      val exploreOrigin = localAssumePathWithOrigin.last
      val dij = shortestPath.from(exploreOrigin)

      val toExplore = dij.nodesByDistance.filter(nodesToExplore.contains(_))

      // Do a quick computation to have something to return now
      var explorationPathWithOrigin: List[Node[NodeStatusType]] = if (toExplore.isEmpty) localAssumePathWithOrigin else localAssumePathWithOrigin ++ dij.pathTo(toExplore.head).get

      var explorationPathLength: Double = _

      var isInterrupted = false

      val future = Future({
        val assumePathDistance = localAssumePathWithOrigin.zip(localAssumePathWithOrigin.tail).map { case (start, end) => start.outNeighbors(end).cost }.sum

        val adjustedToExplore = mutable.Set.empty[Node[NodeStatusType]] ++ toExplore
        for (node <- localAssumePathWithOrigin) {
          adjustedToExplore --= nodesInView(node)
        }

        explorationPathLength = Double.MaxValue

        doComputation(localAssumePathWithOrigin.reverse.tail, assumePathDistance, "", exploreOrigin, null, adjustedToExplore.toSet, backtrackingMaxCount)
      })

      def interrupt(): Unit = {
        isInterrupted = true
      }

      def doComputation(pathSoFarReversed: List[Node[NodeStatusType]], distanceSoFar: Double, signature: String, currentNode: Node[NodeStatusType], previousNode: Node[NodeStatusType], toExplore: Set[Node[NodeStatusType]], backtrackingLimit: Int): Unit = {
        var signatureVar = signature
        var toExploreVar = toExplore
        var currentNodeVar = currentNode
        var previousNodeVar = previousNode
        var distanceSoFarVar = distanceSoFar
        var pathSoFarReversedVar = pathSoFarReversed

        var straightPath = true

        while (!isInterrupted && straightPath) {
          // Repeat as long as we go along a path without any alternatives

          val nodesSeen = nodesInView(currentNodeVar) // Removes nodes from toExplore that we can see from the currentNode
          toExploreVar = toExploreVar -- nodesSeen

          val exploreMaxCountWithBacktrackingLimit = if (backtrackingLimit == 0) 1 else exploreMaxCount

          if (toExploreVar.isEmpty) {
            // println("+ " + signatureVar)
            pathSoFarReversedVar = currentNodeVar :: pathSoFarReversedVar

            val resultingPath = pathSoFarReversedVar.reverse
            if (resultingPath.startsWith(assumePathWithOrigin)) {
              explorationPathWithOrigin = resultingPath
              explorationPathLength = distanceSoFarVar
            }
            straightPath = false

          } else {
            // Select a few closes nodes that are to be explored and assemble an array of nodes we should go to
            // in order to get to the selected nodes to be explored
            val dij = shortestPath.from(currentNodeVar)

            val nodesByDistanceIter = dij.nodesByDistance.iterator
            var toExploreCount = 0

            var previousNodeIsIncluded = false

            val neighborsToExplore = new Array[Node[NodeStatusType]](exploreMaxCountWithBacktrackingLimit)
            var neighborsToExploreLen = 0

            while (toExploreCount < exploreMaxCountWithBacktrackingLimit && nodesByDistanceIter.hasNext) {
              val node = nodesByDistanceIter.next

              if (toExploreVar.contains(node)) {
                val neighbor = dij.pathTo(node).get.head
                toExploreCount = toExploreCount + 1

                if (neighbor == previousNodeVar) {
                  // We include previous node only if there is no other node to go to. This is an optimization that prevents going back if there is something along the path we have already chosen
                  previousNodeIsIncluded = true

                } else if (!neighborsToExplore.contains(neighbor)) {
                  neighborsToExplore(neighborsToExploreLen) = dij.pathTo(node).get.head
                  neighborsToExploreLen = neighborsToExploreLen + 1
                }
              }
            }

            if (neighborsToExploreLen == 0) {
              neighborsToExplore(neighborsToExploreLen) = previousNodeVar
              neighborsToExploreLen = neighborsToExploreLen + 1
            }

            // Now neighborsToExplore contains a neighbors to go to in order to get to selected nodes to be explored
            if (neighborsToExploreLen == 1) {
              val node = neighborsToExplore(0)
              val distanceToNode = currentNodeVar.outNeighbors(node).cost

              if (distanceToNode + distanceSoFarVar < explorationPathLength) {
                pathSoFarReversedVar = currentNodeVar :: pathSoFarReversedVar
                distanceSoFarVar = distanceSoFarVar + distanceToNode
                signatureVar = signatureVar + "1"
                previousNodeVar = currentNodeVar
                currentNodeVar = node

                straightPath = true

              } else {
                straightPath = false
              }

            } else {

              // There are more neighborsToExplore, thus we recursively explore all and select the shortest path

              var signatureDigit = neighborsToExploreLen

              for (node <- neighborsToExplore if node != null) {
                val distanceToNode = currentNodeVar.outNeighbors(node).cost

                if (distanceToNode + distanceSoFarVar < explorationPathLength) {
                  doComputation(currentNodeVar :: pathSoFarReversedVar, distanceSoFarVar + distanceToNode, signatureVar + signatureDigit, node, currentNodeVar, toExploreVar, backtrackingLimit / neighborsToExploreLen)
                }

                signatureDigit = signatureDigit - 1
              }

              straightPath = false
            }
          }
        }
      }
    }

    def assume(path: List[Node[NodeStatusType]]): Unit = {
      val pathWithOrigin = walkedPathWithOrigin ++ path
      require(pathWithOrigin.startsWith(assumePathWithOrigin))

      assumePathWithOrigin = pathWithOrigin
    }

    def walked(path: List[Node[NodeStatusType]]): Unit = {
      val pathWithOrigin = walkedPathWithOrigin ++ path

      if (!assumePathWithOrigin.startsWith(pathWithOrigin)) {
        // We walked beyond assumed path. Thus assume this longer path.
        // Checking of prefix equality is part of the "assume" method. This may potentially restart the search.
        assume(path)
      }

      walkedPathWithOrigin = pathWithOrigin
    }

    def origin: Node[NodeStatusType] = walkedPathWithOrigin.last

    def explorationPath: List[Node[NodeStatusType]] = {

      var result: List[Node[NodeStatusType]] = null

      if (currentTask != null) {
        val currentExplorationPath = currentTask.explorationPathWithOrigin

        if (currentExplorationPath.startsWith(assumePathWithOrigin)) {
          result = currentExplorationPath.drop(walkedPathWithOrigin.size)
        }
      }

      if (result == null) {
        if (currentTask != null) {
          currentTask.interrupt()
        }

        currentTask = new ComputationTask

        result = currentTask.explorationPathWithOrigin.tail
      }

      result
    }

  }
}

case class BuildingStatus(temperature: Int, brokenness: Int, fieryness: Int) extends RCRSNodeStatus

case class Position(x: Double, y: Double) {
  def distanceTo(other: Position): Double = math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
}

/**
  * Message sent from brigade to central agent
  */
case class FireBrigadeToInitiator(mirrorState: MirrorState, position: Position, currentAreaID: EntityID, statusMap: Map[Int, RCRSNodeStatus]) extends Message

object FireBrigadeToInitiator {
  val buildingStatusCodec = {
    (constant(bin"01")) ::
      ("temperature" | uint(10)) ::
      ("brokenness" | uint8) ::
      ("fieryness" | uint4)
  }.as[BuildingStatus].upcast[RCRSNodeStatus]

  val closeIdxCodec = uint(16)

  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.FIRE_BRIGADE_TO_INITIATOR.id, Message.MessageTypeBits))) ::
      ("state" | Message.fireBrigadeStateCodec) ::
      ("position" | Message.positionCodec) ::
      ("currentAreaId" | Message.entityIDCodec) ::
      ("statusMap" | listOfN(uint(6), ("closeIdx" | closeIdxCodec) ~ buildingStatusCodec).xmap[Map[Int, RCRSNodeStatus]](_.toMap, _.toList))
  }.as[FireBrigadeToInitiator]
}

abstract class Message

object Message {
  val MessageTypeBits = 4

  object MessageType extends Enumeration {
    type MessageType = Value

    val FIRE_BRIGADE_TO_INITIATOR = Value(0)
    val INITIATOR_TO_FIRE_BRIGADE = Value(1)
  }

  val entityIDCodec = int32.xmap((id: Int) => new EntityID(id), (id: EntityID) => id.getValue)
  val positionCodec = (double :: double).as[Position]
  val fireBrigadeStateCodec = enumerated(uint4, MirrorState)


  val codec = choice(
    FireBrigadeToInitiator.codec.upcast[Message],
    InitiatorToFireBrigade.codec.upcast[Message]
  )

  def decode(bytes: Array[Byte]): Message = {
    val bits = BitVector(bytes)

    codec.decodeValue(bits) match {
      case Successful(msg) => msg
      case Failure(_) => null
    }
  }

  def encode(msg: Message): Array[Byte] = {
    codec.encode(msg).require.toByteArray
  }
}

/**
  * Message sent from central agent to brigade
  */
case class InitiatorToFireBrigade(receiverId: EntityID, state: MirrorState, asignedFireLocation: Option[EntityID]) extends Message

object InitiatorToFireBrigade {
  val codec = {
    (constant(BitVector.fromInt(Message.MessageType.INITIATOR_TO_FIRE_BRIGADE.id, Message.MessageTypeBits))) ::
      ("receiverId" | Message.entityIDCodec) ::
      ("state" | Message.fireBrigadeStateCodec) ::
      ("assignedFireLocation" | optional(bool, Message.entityIDCodec))
  }.as[InitiatorToFireBrigade]
}

object Constants {
  val TO_STATION: Int = 1
  val TO_AGENTS: Int = 2
}


object RCRSMapStatic {
  import java.io._

  import scala.collection.JavaConverters._

  private var initialized = false

  val lineOfSight = mutable.Map.empty[EntityID, Set[EntityID]]

  case class CloseAreaIDs(byIdx: Map[Int, EntityID], byAreaId: Map[EntityID, Int])
  val closeAreaIDs = mutable.Map.empty[EntityID, CloseAreaIDs]

  private def computeLineOfSight(config: Config, model: StandardWorldModel): Unit = {
    println("Computation of lines of sight...")

    val modelIterable = model.asScala

    val entityCount = modelIterable.size
    var entityIndex = 0

    for (entity <- modelIterable) {
      entity match {
        case area: Area =>
          val los = new LineOfSightNoSolver(config, model)
          val visibleEntities = los.getVisibleEntities(Position(area.getX, area.getY)).asScala.collect{ case visibleArea: Area => visibleArea.getID }.toSet
          lineOfSight += (area.getID -> visibleEntities)
        case _ =>
      }

      entityIndex = entityIndex + 1

      if (entityIndex % 100 == 0) {
        println(s"  $entityIndex / $entityCount")
      }
    }
  }

  private def computeCloseAreaIDs(config: Config, model: StandardWorldModel): Unit = {
    println("Computation of short area-based indexes...")

    val areas = model.asScala.collect{ case area: Area => area }.toList

    for (area <- areas) {
      val areaPos = Position(area.getX, area.getY)
      val cAreas = areas.sortBy(a => areaPos.distanceTo(Position(a.getX, a.getY))).map(_.getID).zipWithIndex
      closeAreaIDs += area.getID -> CloseAreaIDs(cAreas.map(_.swap).toMap, cAreas.toMap)
    }
  }

  def initialize(config: Config, model: StandardWorldModel): Unit = {
    val precomputeFileName = "precompute.data"

    if (!initialized) {
      // try to load map from file
      var input: ObjectInputStream = null
      try {
        input = new ObjectInputStream(new FileInputStream(precomputeFileName))

        lineOfSight.clear()
        lineOfSight ++= input.readObject().asInstanceOf[mutable.Map[Int, Set[Int]]]
          .map{ case (key, value) => new EntityID(key) -> value.map(valueID => new EntityID(valueID)) }  // transforms Map[Int, Set[Int]] to Map[EntityID, Set[EntityID]]

        closeAreaIDs.clear()
        closeAreaIDs ++= input.readObject().asInstanceOf[mutable.Map[Int, (Map[Int,Int], Map[Int,Int])]]
          .map{ case (refAreaId, (byIdx, byAreaId)) => new EntityID(refAreaId) -> CloseAreaIDs(byIdx.mapValues(new EntityID(_)), byAreaId.map{ case (id, idx) => new EntityID(id) -> idx }) }

        println(s"Loaded precomputed data from '${precomputeFileName}', size: ${closeAreaIDs.size}")
        initialized = true
      } catch {
        case _: FileNotFoundException => println(s"File with precomputed data '${precomputeFileName}' not found")
      } finally {
        if (input != null) {
          input.close()
        }
      }
    }

    if (!initialized) {
      computeLineOfSight(config, model)
      computeCloseAreaIDs(config, model)

      // try to save map to file
      var output: ObjectOutputStream = null
      try {
        val output = new ObjectOutputStream(new FileOutputStream(precomputeFileName))

        output.writeObject(lineOfSight.map{ case (key, value) => (key.getValue, value.map(_.getValue)) })  // transforms Map[EntityID, Set[EntityID]] to Map[Int, Set[Int]]

        output.writeObject(closeAreaIDs.map{
          case (refAreaId, CloseAreaIDs(byIdx, byAreaId)) =>
            refAreaId.getValue -> (byIdx.mapValues(_.getValue).map(identity), byAreaId.map{ case (id, idx) => id.getValue -> idx })
        })

        println(s"Saved precomputed data to \'${precomputeFileName}\'")
      } finally {
        if (output != null) {
          output.close
        }
      }

      println(s"  finished")
      initialized = true
    }
  }

}
