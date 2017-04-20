package tcof

import rescuecore2.log.Logger

class RootEnsembleAnchor[EnsembleType <: RootEnsemble] private[tcof](val builder: () => EnsembleType) {
  def initiate() = {
    init()

//    Logger.info(s"RootEnsembleAnchor init called")

    while (solve()) {
//      Logger.info(s"RootEnsembleAnchor utility: ${instance.toStringWithUtility}")
    }

//    Logger.info(s"RootEnsembleAnchor utility finished")

    commit()
//    Logger.info(s"RootEnsembleAnchor commit called")
  }

  private var _solution: EnsembleType = _

  def instance: EnsembleType = _solution

  def init(): Unit = {
    _solution = builder()

    // This is not needed per se because ensembles are discarded in each step anyway. However, component are not. We keep it here for uniformity with components.
    val config = new Config(new SolverModel())
    for (stage <- InitStages.values) {
      _solution._init(stage, config)
    }

    instance._executePreActions()
  }

  def solve(): Boolean = _solution._solverModel.solveAndRecord()

  def commit(): Unit = {
    instance._executeActions()
  }
}

