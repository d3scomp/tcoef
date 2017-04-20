package rcrs

import rcrs.comm._
import rcrs.scenario.ProtectScenario
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade => RescueFireBrigade, _}
import rescuecore2.worldmodel.ChangeSet
import tcof.traits.map2d.Position


class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  val scenario = new ProtectScenario(this)
  // initialization moved to postConnect (agent.model is null in constructor)
  var component: scenario.FireStation = _
  var fireBrigadeComponents: Iterable[scenario.FireBrigade] = _

  override protected def postConnect() {
    Logger.info(s"Central agent connected")

    // component initialization
    component = createFireStationComponent
    fireBrigadeComponents = createFireBrigadeComponents
    scenario.components = component +: fireBrigadeComponents.toList

    super.postConnect()
    scenario.init()
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    Logger.info(s"CentralAgent: Think called at time $time - START")
    super.think(time, changes, heard)

    if (time == ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_STATION)
    }

    if (time >= ignoreAgentCommandsUntil) {
      scenario.rcrsStep(time: Int, changes: ChangeSet, heard: List[Command])

      // solve component
      component.init()

      val startTime = System.currentTimeMillis()

      while (component.solve()) {
        //println(component.toStringWithUtility)
        //Logger.info(s">>>> ${component.toStringWithUtility}")
      }

      val endTime = System.currentTimeMillis()
      val timeElapsed = endTime - startTime

      Logger.info(s"CentralAgent: Think called at time $time - END, think took $timeElapsed")

      component.commit()
    }
  }

  /**
    * Creates FireBrigade components and assigns them position from configuration.
    */
  private def createFireBrigadeComponents: Iterable[scenario.FireBrigade] = {
    findEntities[RescueFireBrigade](StandardEntityURN.FIRE_BRIGADE).map { fb =>
      val model = rcrsAgent.delegateModel
      val location = fb.getLocation(model)
      new scenario.FireBrigade(fb.getID, Position(location.first.toDouble, location.second.toDouble))
    }
  }

  private def createFireStationComponent: scenario.FireStation = {
    // TODO - assumption - exactly one fire station exists
    val fs = findEntities[FireStation](StandardEntityURN.FIRE_STATION).head
    new scenario.FireStation(fs.getID)
  }

  private def findEntities[T <: StandardEntity](urn: StandardEntityURN): Iterable[T] = {
    import scala.collection.JavaConverters._
    model.getEntitiesOfType(urn).asScala
      .map{_.asInstanceOf[T]}
  }

  // TODO - central agent serves as fire station - but it probably should be just
  // abstract class and code for fire station should be in derived class
  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(
    StandardEntityURN.FIRE_STATION
//    StandardEntityURN.AMBULANCE_CENTRE,
//    StandardEntityURN.POLICE_OFFICE
  )
}
