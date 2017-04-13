package rcrs.scenario

import rcrs.traits.map2d.{RCRSNodeStatus, BuildingStatus}
import rescuecore2.standard.entities.{Building, StandardWorldModel, StandardEntityURN, StandardEntity}
import rescuecore2.worldmodel.EntityID
import tcof.traits.map2d.{Map2D, Node}
import tcof.traits.statespace.interpolate

object ScenarioUtils {

  def findEntities[T <: StandardEntity](model: StandardWorldModel, urn: StandardEntityURN): Iterable[T] = {
    import scala.collection.JavaConverters._
    model.getEntitiesOfType(urn).asScala
      .map{_.asInstanceOf[T]}
  }

  def travelTimeToUtility(routeTime: Option[Double]): Int = routeTime match {
    case None => 0
    case Some(time) => 100 - time.toInt
  }

  def burnModel(node: Node[BuildingStatus]) = interpolate.linear(
    0.0 -> 0.0,
    0.5 -> 0.1,
    1.0 -> 0.0
  )

}
