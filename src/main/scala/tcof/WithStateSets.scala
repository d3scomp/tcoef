package tcof

import tcof.InitStages.InitStages
import tcof.Utils._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait WithStateSets extends Initializable {
  this: WithConfig =>

  /** A set of all potential ensembles */
  private[tcof] var _allStates: ListBuffer[State] = _

  def State: State = State(randomName)
  def State(name: String): State = {
    val state = new State(_genStateId, name)
    _allStates += state
    state
  }

  def State[StateType <: State](stateFirst: StateType, stateRest: StateType*): StateSet[StateType] = State(randomName, stateRest.+:(stateFirst))
  def State[StateType <: State](name: String, stateFirst: StateType, stateRest: StateType*): StateSet[StateType] = State(name, stateRest.+:(stateFirst))
  def State[StateType <: State](name: String, stats: Iterable[StateType]): StateSet[StateType] = {
    val stateSet = new StateSet(_genStateId, name, new StateSetMembers(stats))
    _allStates += stateSet
    stateSet
  }

  def StateOr[StateType <: State](stateFirst: StateType, stateRest: StateType*): StateSetOr[StateType] = StateOr(randomName, stateRest.+:(stateFirst))
  def StateOr[StateType <: State](name: String, stateFirst: StateType, stateRest: StateType*): StateSetOr[StateType] = StateOr(name, stateRest.+:(stateFirst))
  def StateOr[StateType <: State](name: String, stats: Iterable[StateType]): StateSetOr[StateType] = {
    val stateSet = new StateSetOr(_genStateId, name, new StateSetMembers(stats))
    _allStates += stateSet
    stateSet
  }

  def StateAnd[StateType <: State](stateFirst: StateType, stateRest: StateType*): StateSetAnd[StateType] = StateAnd(randomName, stateRest.+:(stateFirst))
  def StateAnd[StateType <: State](name: String, stateFirst: StateType, stateRest: StateType*): StateSetAnd[StateType] = StateAnd(name, stateRest.+:(stateFirst))
  def StateAnd[StateType <: State](name: String, stats: Iterable[StateType]): StateSetAnd[StateType] = {
    val stateSet = new StateSetAnd(_genStateId, name, new StateSetMembers(stats))
    _allStates += stateSet
    stateSet
  }

  private[tcof] var _stateNextId = 0
  private[tcof] def _genStateId = {
    val result = _stateNextId
    _stateNextId = _stateNextId + 1
    result
  }

  private[tcof] var _rootState: StateSet[State] = _
  def rootStates: StateSet[State] = _rootState

  private[tcof] var _states: StateSetEquiv[State] = _
  def states: StateSetEquiv[State] = _states


  override private[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.EraseAllStates =>
        _allStates = mutable.ListBuffer.empty[State]

      case InitStages.ExtraDeclarations =>
        _rootState = State("<root>", _allStates.filter(_.parent == null))

        val linkedMembers = _allStates.filterNot(_ == _rootState).map(s => new LinkedMember(s, s.parent, s.indexInParent))

        _states = new StateSetEquiv(new StateSetMembersEquiv(linkedMembers))
      case _ =>
    }

    // TODO - this is not very pretty but I couldn't come up with better solution
    if (stage >= InitStages.ExtraDeclarations) {
      _states._init(stage, config)

      _allStates.foreach {
        case x: Initializable => x._init(stage, config)
        case _ =>
      }
    }
  }

  implicit def stateToLogical(state: State): Logical = LogicalBoolVar(_solverModel.member(state.indexInParent, state.parent.allMembersVar).reify())
}


