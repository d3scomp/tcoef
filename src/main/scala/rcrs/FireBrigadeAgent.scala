package rcrs

import tcof.traits.map2d.Map2DTrait
import rcrs.comm._
import rcrs.scenario.RescueScenario
import rcrs.traits.map2d.{RCRSMapAdapterTrait, RCRSNodeStatus}
import rescuecore2.messages.Command
import rescuecore2.standard.entities.{StandardEntityURN, FireBrigade => FireBrigadeEntity}
import rescuecore2.worldmodel.ChangeSet


class FireBrigadeAgent extends ScalaAgent {
  override type AgentEntityType = FireBrigadeEntity

  val scenario = new RescueScenario(this)
  // TODO - number, position - assign number from central agent?
  // assign current position?
  val component = new scenario.FireBrigade(0, null)

  /*
  private val MAX_WATER_KEY = "fire.tank.maximum"
  private val MAX_DISTANCE_KEY = "fire.extinguish.max-distance"
  private val MAX_POWER_KEY = "fire.extinguish.max-sum"

  private var maxWater: Int = _
  private var maxDistance: Int = _
  private var maxPower: Int = _
*/

  override protected def postConnect() {
    super.postConnect()
    scenario.init()

    /*
    maxWater = config.getIntValue(MAX_WATER_KEY)
    maxDistance = config.getIntValue(MAX_DISTANCE_KEY)
    maxPower = config.getIntValue(MAX_POWER_KEY)

    Logger.info(s"Fire brigade agent connected: max extinguish distance = $maxDistance, max power = $maxPower, max tank = $maxWater")
    */
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
      scenario.components = List(component)

      component.init()

      while (component.solve()) {
        println(component.toStringWithUtility)
      }

      component.commit()

    }
  }

  override protected def getRequestedEntityURNs: List[StandardEntityURN] = List(StandardEntityURN.FIRE_BRIGADE)
}
