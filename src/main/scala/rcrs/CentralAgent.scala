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

  val scenario = new RescueScenario(this)

  override protected def postConnect() {
    Logger.info(s"Central agent connected")
    super.postConnect()

    // TODO - numbers in components would differ on different CentralAgents
    // - if this is a problem, they should be either obtained from some another (single)
    //  central component or some convention for translation from (rescue) EntityID to
    //  number should be used
    val component = createFireStationComponent
    val fireBrigadeComponents = createFireBrigadeComponents

    // TODO - inject components somehow?
    scenario.components = component +: fireBrigadeComponents.toList
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

      // TODO - central agent now handles whole rootEnsemble, but in rcrs
      // there may be multiple central agents
      scenario.rootEnsemble.init()
      println("RescueScenario initialized")

      while (scenario.rootEnsemble.solve()) {
        println(scenario.toString)
      }

      // TODO - ensemble sets zone, but this happens only on central agent.
      // Where is the message sent to mobile agent?
      scenario.rootEnsemble.commit()
    }
  }

  /**
    * Creates FireBrigade components and assigns them position from configuration.
    */
  private def createFireBrigadeComponents: Iterable[scenario.FireBrigade] = {
    val fireBrigades = findEntities[RescueFireBrigade](StandardEntityURN.FIRE_BRIGADE)

    fireBrigades.zipWithIndex.map { case (fb, index) =>
      new scenario.FireBrigade(index, Position(fb.getX, fb.getY))
    }
  }

  private def createFireStationComponent: scenario.FireStation = {
    // TODO - assumption - exactly one fire station exists
    val fs = findEntities[FireStation](StandardEntityURN.FIRE_STATION).head
    new scenario.FireStation(0, Position(fs.getX, fs.getY))
  }

  private def findEntities[T <: StandardEntity](urn: StandardEntityURN): Iterable[T] = {
    import scala.collection.JavaConverters._
    model.getEntitiesOfType(urn).asScala
      .map{_.asInstanceOf[T]}
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(
    StandardEntityURN.FIRE_STATION
//    StandardEntityURN.AMBULANCE_CENTRE,
//    StandardEntityURN.POLICE_OFFICE
  )
}
