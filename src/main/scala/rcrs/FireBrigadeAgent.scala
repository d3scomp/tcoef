package rcrs

import rescuecore2.log.Logger
import rcrs.comm._
import rcrs.scenario.{ProtectScenario, RescueScenario}
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{StandardEntityURN, FireBrigade => FireBrigadeEntity}
import rescuecore2.worldmodel.ChangeSet
import tcof.traits.map2d.Position


class FireBrigadeAgent extends ScalaAgent {
  override type AgentEntityType = FireBrigadeEntity

  private val scenario = new ProtectScenario(this)
  private lazy val component: scenario.FireBrigade = createFireBrigade() // lazy to postpone creation (getID returns null as model is not initialized)

  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  lazy val maxWater = config.getIntValue(MAX_WATER_KEY)
  lazy val maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
  lazy val maxPower = config.getIntValue(MAX_POWER_KEY)

  override protected def postConnect() {
    super.postConnect()

    scenario.components = List(component)
    scenario.init()

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")
  }

  override def think(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    super.think(time, changes, heard)
    //Logger.info(s"FireBrigadeAgent: Think called at time $time. Position ${getPosition}")

    if (time == ignoreAgentCommandsUntil) {
      // Logger.info("Subscribing to channels")
      sendSubscribe(time, Constants.TO_AGENTS)
    }

    if (time >= ignoreAgentCommandsUntil) {
      // Logger.info("Heard: " + heard)

      scenario.rcrsStep(time: Int, changes: ChangeSet, heard: List[Command])

      component.init()

      while (component.solve()) {
//        println(component.toStringWithUtility)
      }

      component.commit()

    }
  }

  private def createFireBrigade(): scenario.FireBrigade = {
    val model = rcrsAgent.delegateModel
    val location = rcrsAgent.delegateLocation.getLocation(model)
    val position = Position(location.first.toDouble, location.second.toDouble)
    new scenario.FireBrigade(getID, position)
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}