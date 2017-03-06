package tcof

import scala.collection.mutable

trait WithActionsInComponent extends WithActions {
  this: Component =>

  def ensembleResolution(act: => Unit): Unit = _ensembleResolutionActions += act _

  private[tcof] var _ensembleResolutionActions = mutable.ListBuffer.empty[() => Unit]

  private[tcof] override def _executePreActions(): Unit = _preActions.foreach(_())
  private[tcof] def _executeEnsembleResolutionActions(): Unit = _ensembleResolutionActions.foreach(_())
  private[tcof] override def _executeActions(): Unit = _actions.foreach(_())
}
