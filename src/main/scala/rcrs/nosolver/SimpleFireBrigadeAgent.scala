package rcrs.nosolver

import rcrs.scenario.MirrorState
import rcrs.scenario.MirrorState.{ProtectingMirror, RefillingMirror}
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities._
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.{ChangeSet, EntityID}

class SimpleFireBrigadeAgent extends ScalaAgent with Map2DTrait[RCRSNodeStatus] with RCRSMapAdapterTrait /* added inherit map methods */{
  override type AgentEntityType = FireBrigade

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  lazy val maxWater = config.getIntValue(MAX_WATER_KEY)
  lazy val maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
  lazy val maxPower = config.getIntValue(MAX_POWER_KEY)

  override protected def postConnect() {
    super.postConnect()
    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")

    this.init()
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    super.think(time, changes, heard)
    //Logger.info(s"FireBrigadeAgent: Think called at time $time. Position ${getPosition}")

    if (time == ignoreAgentCommandsUntil) {
      // Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_AGENTS)
    }

    if (time >= ignoreAgentCommandsUntil) {

      // sensing - process heard messages
      processReceivedMessages(heard)

      // solve
      // override state received from central agent if out of water
      if (tankEmpty || refillingAtRefuge) {
        brigadeState = MirrorState.RefillingMirror
      }

      // perform action
      performAction(time)

      // coordination - send message to initiator
      sendMessages(changes, time)
    }
  }

  private var brigadeState = MirrorState.IdleMirror
  private var assignedFireLocation: Option[EntityID] = None

  private def processReceivedMessages(heard: List[Command]): Unit = {
    heard.foreach {
      case speak : AKSpeak =>
        val msg = Message.decode(speak.getContent)
        msg match {
          case InitiatorToFireBrigade(receiverId, mirrorState, fireLocation) if receiverId == this.rcrsAgent.getID =>
            brigadeState = mirrorState
            assignedFireLocation = fireLocation

          case _ =>
        }

      case _ =>
    }
  }

  private def sendMessages(changes: ChangeSet, time: Int): Unit = {
    val statusMap = collectStatusChanges(changes)
    val brigadePositionEntityID = me.getPosition
    val brigadePosition = Position(me.getX, me.getY)
    val message = FireBrigadeToInitiator(brigadeState, brigadePosition, brigadePositionEntityID, statusMap)
//    Logger.info(s"brigade ${me.getID} sending ${message}")
    sendSpeak(time, Constants.TO_STATION, Message.encode(message))
  }

  private def collectStatusChanges(changes: ChangeSet): Map[Int, RCRSNodeStatus] = {
    import scala.collection.JavaConverters._

    changes.getChangedEntities.asScala
      .map(entityID => (entityID, model.getEntity(entityID)))
      .collect {
        case (entityID, _: Building) =>
          val temperature = changes.getChangedProperty(entityID, StandardPropertyURN.TEMPERATURE.toString).getValue.asInstanceOf[Int]
          val brokenness = changes.getChangedProperty(entityID, StandardPropertyURN.BROKENNESS.toString).getValue.asInstanceOf[Int]
          val fieryNess = changes.getChangedProperty(entityID, StandardPropertyURN.FIERYNESS.toString).getValue.asInstanceOf[Int]
          entityID.getValue -> BuildingStatus(temperature, brokenness, fieryNess)
      }.toMap
  }

  private def performAction(time: Int): Unit = {
    brigadeState match {
      case RefillingMirror if !refillingAtRefuge =>
        moveTo(nearestRefuge, time)

      case ProtectingMirror =>
        if (inExtinguishingDistanceFromFire) {
          extinguish(time)
        } else {
          moveTo(assignedBuildingOnFire, time)
        }

      case _ =>
        rest(time)
    }
  }

  private def moveTo(node: Node[RCRSNodeStatus], time: Int) = {
    // TODO - recomputes shortest path in every step
    val currentNode = map.toNode(agent.currentAreaId)
    val path = map.shortestPath.from(currentNode).pathTo(node)
    val entityIdPath = map.toAreaID(path.get)

//    Logger.info(s"brigade ${me.getID} moving to node ${node}")
    sendMove(time, entityIdPath)
  }

  private def rest(time: Int): Unit = {
//    Logger.info(s"brigade ${me.getID} resting")
    sendRest(time)
  }

  private def extinguish(time: Int): Unit = {
//    Logger.info(s"brigade ${me.getID} extinguishing")
    sendExtinguish(time, assignedFireLocation.get, maxPower)
  }

  private def assignedBuildingOnFire: Node[RCRSNodeStatus] = {
    map.toNode(assignedFireLocation.get)
  }

  private def inExtinguishingDistanceFromFire: Boolean = {
    val brigadePosition = Position(me.getX, me.getY)
    brigadePosition.distanceTo(assignedBuildingOnFire.center) < maxDistance
  }

  private def nearestRefuge: Node[RCRSNodeStatus] = {
    // TODO - now picks first found refuge
    import collection.JavaConverters._
    val refuge = agent.model.getEntitiesOfType(StandardEntityURN.REFUGE).asScala.head.asInstanceOf[Refuge]
    map.toNode(refuge.getID)
  }

  private def refillingAtRefuge: Boolean = location.isInstanceOf[Refuge] && me.getWater < maxWater
  private def tankEmpty: Boolean = me.getWater == 0

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)

  override def agent: ScalaAgent = this
}
