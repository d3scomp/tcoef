package rcrs.scenario

import rcrs.comm._
import rescuecore2.log.Logger
import tcof.traits.map2d.{Map2D, Map2DTrait, Node, Position}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus
import rescuecore2.standard.entities.Human
import tcof.Model

trait AreaExplorationSupport {
  this: Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] =>

  case class MapZone(xIdx: Int, yIdx: Int, maxLastVisitTime: Int) {
    override def toString: String = s"MapZone($xIdx, $yIdx)"

    val toExplore: Set[Node[RCRSNodeStatus]] = map.nodes.filter(node =>
      node.center.x >= xIdx * MapZone.xTileSize &&
        node.center.y >= yIdx * MapZone.yTileSize &&
        node.center.x < (xIdx + 1) * MapZone.xTileSize &&
        node.center.y <= (yIdx + 1) * MapZone.yTileSize &&
        node.lastVisitTime < maxLastVisitTime
    ).toSet

    val leftBottom = new Position(xIdx * MapZone.xTileSize, yIdx * MapZone.yTileSize)
    val rightTop = new Position((xIdx + 1) * MapZone.xTileSize, (yIdx + 1) * MapZone.yTileSize)
    val center = new Position((xIdx + 0.5) * MapZone.xTileSize, (yIdx + 0.5) * MapZone.yTileSize)
  }

  object MapZone {
    val xTileSize = 250000
    val yTileSize = 200000
  }

  /**
    * Follows the exploration path to traverse the assigned map zone (in variable areaExplorationAssignedZone)
    */
  trait AreaExploration {

    this: MobileUnitComponent#MobileUnit =>

    var areaExplorationAssignedZone: MapZone = _
    var areaExploration: Map2D[RCRSNodeStatus]#AreaExploration = _
    var areaExplorationCurrentZone: MapZone = _

    val AreaExploration = State

    preActions {
    }

    constraints(
      AreaExploration -> (areaExplorationAssignedZone != null)
    )

    actions {
      states.selectedMembers.foreach {
        case AreaExploration => doExploration()
        case _ =>
      }
    }

    def doExploration(): Unit = {
      val assumeLen = 10

      if (areaExploration == null || areaExplorationCurrentZone != areaExplorationAssignedZone) {
        areaExplorationCurrentZone = areaExplorationAssignedZone
        areaExploration = map.rcrsAreaExploration(map.currentNode, areaExplorationAssignedZone.toExplore)

      } else {
        val path = areaExploration.explorationPath
        val history = agentAs[Human].me.getPositionHistory.toList.grouped(2).map{ case List(x,y) => Position(x,y) }.toList

        val walkedPath = map.getWalkedPath(areaExploration.origin, path, history)

        // Logger.info("Walked segment: " + walkedPath.map(map.toArea).toString)
        areaExploration.walked(walkedPath)
      }

      val path = areaExploration.explorationPath
      val assumePath = path.slice(0,assumeLen)

      // Logger.info("Assuming segment: " + assumePath.map(map.toArea).toString)
      areaExploration.assume(assumePath)

      // Logger.info("Walking path: " + path.map(map.toArea).toString)
      agent.sendMove(time, map.toAreaID(path))
    }

  }


  /**
    * Stores information about world updated from ExplorationStatus messages.
    */
  trait AreaExplorationCentral {

    this: CentralUnitComponent#CentralUnit =>

    preActions {
      sensing.messages.foreach {
        case (ExplorationStatus(referenceAreaId, statusMap), _) =>
          val nodes = statusMap.map{
            case (idx, status) =>
              map.toNode(map.closeAreaIDs(referenceAreaId).byIdx(idx)) -> status
          }

          map.nodeStatus ++= nodes

          Logger.info(s"Exploration status updated for: ${nodes.keys.map(map.toArea)}")

        case _ =>
      }
    }
  }
}