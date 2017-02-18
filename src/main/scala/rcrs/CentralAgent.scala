package rcrs

import rcrs.comm._
import rcrs.scenario.RescueScenario
import rescuecore2.log.Logger
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{FireBrigade => RescueFireBrigade, _}
import rescuecore2.worldmodel.ChangeSet
import tcof.traits.map2d.Position


class CentralAgent extends ScalaAgent {
  override type AgentEntityType = Building

  val scenario: RescueScenario = new RescueScenario(this)
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
    Logger.info(s"CentralAgent: Think called at time $time")
    super.think(time, changes, heard)

    if (time == ignoreAgentCommandsUntil) {
      Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_STATION)
    }

    if (time >= ignoreAgentCommandsUntil) {
      scenario.rcrsStep(time: Int, changes: ChangeSet, heard: List[Command])

      // solve component
      component.init()

      while (component.solve()) {
        println(component.toStringWithUtility)
      }

      component.commit()

      // solve root ensemble

      // TODO - central agent now handles whole rootEnsemble, but in rcrs
      // there may be multiple central agents
      // TODO - remove ensemble init/solve/commit from here?
      // - where is solve/commit called?
      /*
      scenario.rootEnsemble.init()
      println("RescueScenario initialized")

      while (scenario.rootEnsemble.solve()) {
        println(scenario.toString)
      }

      // TODO - ensemble sets zone, but this happens only on central agent.
      // Where is the message sent to mobile agent?
      scenario.rootEnsemble.commit()
      */
    }
  }

  /**
    * Creates FireBrigade components and assigns them position from configuration.
    */
  private def createFireBrigadeComponents: Iterable[scenario.FireBrigade] = {
    findEntities[RescueFireBrigade](StandardEntityURN.FIRE_BRIGADE).map { fb =>
      new scenario.FireBrigade(fb.getID, Position(fb.getX, fb.getY))
    }
  }

  private def createFireStationComponent: scenario.FireStation = {
    // TODO - assumption - exactly one fire station exists
    val fs = findEntities[FireStation](StandardEntityURN.FIRE_STATION).head
    new scenario.FireStation(fs.getID, Position(fs.getX, fs.getY))
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
