package rcrs.traits

import tcof.traits.map2d.Map2DTrait
import rcrs.ScalaAgent
import rcrs.comm.Message
import rcrs.traits.map2d.{RCRSMapAdapterTrait, RCRSNodeStatus}
import rescuecore2.messages.Command
import rescuecore2.standard.entities.StandardEntity
import rescuecore2.standard.messages.AKSpeak
import rescuecore2.worldmodel.ChangeSet
import tcof.Model

trait RCRSConnectorTrait extends RCRSTrait with RCRSMapAdapterTrait {
  this: Model with Map2DTrait[RCRSNodeStatus] =>

  var agent: ScalaAgent = _

  def agentAs[T <: StandardEntity] = agent.asInstanceOf[ScalaAgent {type AgentEntityType = T}]

  object sensing {
    var changes: ChangeSet = _
    var heard: List[Command] = _

    def messages = heard.collect{
      case speak : AKSpeak =>
        (Message.decode(speak.getContent), speak)
    }
  }

  def rcrsStep(time: Int, changes: ChangeSet, heard: List[Command]): Unit = {
    sensing.changes = changes
    sensing.heard = heard

    step(time)
  }

}
