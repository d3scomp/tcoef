package tcof

import tcof.InitStages.InitStages
import tcof.Utils._
import org.chocosolver.solver.Model

import scala.collection.mutable

trait Component extends WithName with WithUtility with WithStateSets with WithActionsInComponent with WithConfig with CommonImplicits with Initializable {
  private[tcof] val _constraintsClauseFuns = mutable.ListBuffer.empty[() => Logical]

  def constraints(clause: => Logical): Unit = {
    _constraintsClauseFuns += clause _
  }

  override protected[tcof] def _init(stage: InitStages, config: Config): Unit = {
    super._init(stage, config)

    stage match {
      case InitStages.RulesCreation =>
        if (_constraintsClauseFuns.nonEmpty)
          _solverModel.post(_solverModel.and(_constraintsClauseFuns.map(_.apply())))

        val sm = _solverModel
        utility match {
          case Some(sm.IntegerIntVar(utilityVar)) => _solverModel.setObjective(Model.MAXIMIZE, utilityVar)
          case _ =>
        }

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

  def solve(): Boolean = _solverModel.solveAndRecord()

  def commit(): Unit = {
    _executeActions()
  }


  override def toString: String =
    s"""Component "$name""""

  def toStringWithUtility: String = {
    s"""Component "$name" (utility: $solutionUtility)${indent(_rootState.toString, 1)}"""
  }

}
