package rcrs.scenario

import tcof.traits.map2d.Position
import tcof.{Component, Universe}

trait MobileUnitComponent {
  this: Universe with ObservationSupport with RegistrationSupport with AreaExplorationSupport with PositionRegistrySupport =>

  abstract class MobileUnit(var position: Position) extends Component with PositionAware with Registration with AreaExploration with Observation with PositionSending{

    val Stopped = State

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

  }

}
