package rcrs.scenario

import rcrs.traits.WithEntityID
import tcof.traits.map2d.Position
import tcof.{Component, Model}

trait MobileUnitComponent {
  this: Model with ObservationSupport with PositionRegistrySupport /* with AreaExplorationSupport with PositionRegistrySupport */ =>

  /**
    * Abstract base for rcrs mobile components
    * @param position
    */
  abstract class MobileUnit(var position: Position) extends Component with WithEntityID with PositionAware /* with AreaExploration */ with Observation with PositionSending {

    //val Stopped = State

    /*
    val Operation = StateOr(Register, AreaExploration, Stopped)

    constraints(
      Operation
    )

    utility(
      states.sum {
        case Observation => 1
        case AreaExploration => 1
        // TODO - during run I get also other values - tcof.State, tcof.StateSetOr
        case _: tcof.State => 0
      }
    )
    */


  }

}
