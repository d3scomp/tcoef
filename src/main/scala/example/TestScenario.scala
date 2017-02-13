package example

import org.apache.commons.math3.distribution.BinomialDistribution
import tcof._
import tcof.traits.map2d.{Map2DTrait, Node, Position}
import tcof.traits.statespace.{StateSpaceTrait, interpolate}
import tcof.traits.statistics.StatisticsTrait

object MapNodeKind extends Enumeration {
  type MapNodeKind = Value
  val Road, Building = Value
}

import example.MapNodeKind.{Building, MapNodeKind}

class MapNodeStatus(var kind: MapNodeKind, var isBurning: Boolean, var burnoutLevel: Double)

class TestScenario extends Model with Map2DTrait[MapNodeStatus] with StateSpaceTrait with StatisticsTrait {

  type MapNode = Node[MapNodeStatus]

  var noOfStatusMsgReceived: Int = _
  var noOfStatusMsgToBeReceived: Int = _

  def burnModel(node: MapNode) = interpolate.linear(
    0.0 -> 0.0,
    0.5 -> 0.1,
    1.0 -> 0.0
  )

  abstract class MobileUnit extends Component {
    var mapPosition: MapNode = _
  }

  class AmbulanceTeam(id: Int) extends MobileUnit {
    name(s"AmbulanceTeam $id")
  }

  class FireBrigade(id: Int) extends MobileUnit {
    name(s"FireBrigade $id")
  }

  val msgDelivery = timeseries.binomial(noOfStatusMsgReceived, noOfStatusMsgToBeReceived - noOfStatusMsgReceived)

  class FireFightingTeam(val building: MapNode) extends Ensemble {
    name(s"FireFightingTeam for building $building")
    val fireBrigades = role("fireBrigades", components.select[FireBrigade])
    val ambulances = role("ambulances", components.select[AmbulanceTeam])
    val allMembers = fireBrigades ++ ambulances

    val routesToBuilding = map.shortestPath.to(building)
    val firePredictor = statespace(burnModel(building), 0, building.status.burnoutLevel)

    /*
    println(s"FireFightingTeam for building $building")
    for (c <- components.collect{ case m: MobileUnit => m}) {
      val d = routesToBuilding.costFrom(c.mapPosition).get
      val f = firePredictor.valueAt(d)
      val s = msgDelivery(time - 3600, time).probability > 0.9 withConfidence 0.95
      println(s"  ${c} - ${d} - ${f} - ${s}")
    }
    */

    membership(
      (msgDelivery(time - 3600, time).probability > 0.9 withConfidence 0.95) &&

      fireBrigades.all(unit => routesToBuilding.costFrom(unit.mapPosition) match {
        case None => false
        case Some(travelTime) => firePredictor.valueAt(travelTime) < 0.9
      }) &&

      fireBrigades.cardinality >= 1 && fireBrigades.cardinality <= 3 // && ambulances.cardinality === 1
    )

    def travelTimeToUtility(routeTime: Option[Double]) = routeTime match {
      case None => 0
      case Some(time) => 100 - time.toInt
    }

    utility(
      allMembers.sum(unit => travelTimeToUtility(routesToBuilding.costFrom(unit.mapPosition)))
    )
  }

  class System extends RootEnsemble {
    val fireFightingTeams = ensembles("FireFightingTeams",
      map.nodes.filter(node => node.status.kind == Building && node.status.isBurning)
        .map(node => new FireFightingTeam(node))
    )

    membership(
      fireFightingTeams.map(_.allMembers).allDisjoint
    )
  }

  val rootEnsemble = root(new System)
}

object TestScenario {
  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    scenario.init()

    val components = List(
      new scenario.FireBrigade(1),
      new scenario.FireBrigade(2),
      new scenario.FireBrigade(3),
      new scenario.AmbulanceTeam(1),
      new scenario.AmbulanceTeam(2)
    )

    scenario.components = components

    val mapNodes = new Array[Array[Node[MapNodeStatus]]](10)

    for (x <- 0 until 10) {
      mapNodes(x) = new Array[Node[MapNodeStatus]](10)
      for (y <- 0 until 10) {
        val node = scenario.map.addNode(Position(x, y))
        node.status = new MapNodeStatus(MapNodeKind.Road, false, 0)
        mapNodes(x)(y) = node
      }
    }

    for (x <- 1 until 10) {
      for (y <- 1 until 10) {
        scenario.map.addDirectedEdge(mapNodes(x)(y), mapNodes(x-1)(y), 1)
        scenario.map.addDirectedEdge(mapNodes(x)(y), mapNodes(x)(y-1), 1)
        scenario.map.addDirectedEdge(mapNodes(x-1)(y), mapNodes(x)(y), 1)
        scenario.map.addDirectedEdge(mapNodes(x)(y-1), mapNodes(x)(y), 1)
      }
    }

    val componentMapPositions = List(
      mapNodes(2)(3),
      mapNodes(0)(8),
      mapNodes(1)(4),
      mapNodes(7)(6),
      mapNodes(9)(2)
    )

    components.zip(componentMapPositions).foreach{
      case (component, position) => component.mapPosition = position
    }

    mapNodes(7)(2).status = new MapNodeStatus(MapNodeKind.Building, true, 0.1)
    mapNodes(3)(4).status = new MapNodeStatus(MapNodeKind.Building, true, 0.7)


    val trials = 3
    val dist = new BinomialDistribution(trials, 0.91)

    scenario.noOfStatusMsgToBeReceived = trials
    for (t <- 0 until 5000) {
      scenario.noOfStatusMsgReceived = dist.sample()
      scenario.step(t)
    }


    scenario.rootEnsemble.init()
    println("System initialized")

    while (scenario.rootEnsemble.solve()) {
      println(scenario.rootEnsemble.instance.toString)
    }

    scenario.rootEnsemble.commit()

    println(scenario.rootEnsemble.instance.solutionUtility)
  }

}

