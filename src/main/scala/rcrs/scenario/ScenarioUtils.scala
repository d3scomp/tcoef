package rcrs.scenario

import rcrs.traits.map2d.BuildingStatus
import rescuecore2.standard.entities._
import tcof.traits.map2d.Node
import tcof.traits.statespace.interpolate

object ScenarioUtils {

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

  def fierynessValue(fieryness: Int) = {
    // TODO
    0.0
  }

}
