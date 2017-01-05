package mpmens

import mpmens.InitStages.InitStages

trait WithUtility extends Initializable {
  private var _utilityFun: Option[() => Integer] = None

  def utility(util: => Integer): Unit = {
    _utilityFun = Some(util _)
  }

  private var _utility: Option[Integer] = null

  private[mpmens] def utility: Option[Integer] = {
    if (_utility == null) {
      _utility = _utilityFun.map(_.apply())
    }

    _utility
  }

  def solutionUtility: Int = _utility match {
    case Some(value) => value.solutionValue
    case None => 0
    case null => 0
  }

  override private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.VariableCreation =>
        _utility = null
      case _ =>
    }
  }
}
