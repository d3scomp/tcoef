package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

class StateSet[+StateType <: State](stateId: Int, name: String, private[tcof] val allMembers: StateSetMembers[StateType]) extends State(stateId, name) with WithMembers[StateType] with Initializable {

  for ((subState, idx) <- allMembers.values.zipWithIndex) {
    require(subState.parent == null)

    subState.parent = this
    subState.indexInParent = idx
  }

  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        if (parent != null) {
          for (idx <- 0 until allMembers.size) {
            _solverModel.ifThen(_solverModel.member(idx, allMembersVar), _solverModel.member(indexInParent, parent.allMembersVar))
          }
        }

      case _ =>
    }
  }


  override def toString: String =
    s"""\nState set "$name""""
}
