package tcof

import tcof.InitStages.InitStages
import tcof.Utils._
import org.chocosolver.solver.Model

import scala.collection.mutable

trait Component extends WithName with WithUtility with WithStateSets with WithActionsInComponent with WithConfig with CommonImplicits with Initializable {
  private[tcof] val _constraintsClauseFuns = mutable.ListBuffer.empty[() => Logical]

  private var initSolver: Boolean = false

  def constraints(clause: => Logical): Unit = {
    _constraintsClauseFuns += clause _
  }

  override protected[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        // constraints cannot be posted to solver at this place - changes to state in preActions would
        // not be reflected in constraints
        initSolver = true

      case _ =>
    }
  }

  def init(): Unit = {
    val config = new Config(new SolverModel())
    for (stage <- InitStages.values) {
      _init(stage, config)
    }

    _executePreActions()
  }

  def solve(): Boolean = {

    // delayed posting of constraints (to reflect changes
    if (initSolver) {
      if (_constraintsClauseFuns.nonEmpty) {
        val constraints = _constraintsClauseFuns.map(_())
        _solverModel.post(_solverModel.and(constraints))
      }

      val sm = _solverModel
      utility match {
        case Some(sm.IntegerIntVar(utilityVar)) => _solverModel.setObjective(Model.MAXIMIZE, utilityVar)
        case _ =>
      }

      initSolver = false
    }

    _solverModel.solveAndRecord()
  }

  def commit(): Unit = {
    _executeActions()
  }


  override def toString: String =
    s"""Component "$name""""

  def toStringWithUtility: String = {
    s"""Component "$name" (utility: $solutionUtility)${indent(_rootState.toString, 1)}"""
  }

}
