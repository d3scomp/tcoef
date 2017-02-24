package rcrs.scenario

import tcof.traits.map2d.Position
import tcof.{Component, Model}

trait CentralUnitComponent {
  this: Model /* with RegistrationSupport with AreaExplorationSupport */ with ObservationSupport with PositionRegistrySupport =>

  abstract class CentralUnit(var position: Position) extends Component
    /* with PositionAware with AreaExplorationCentral with Registrator */ with ObservationReceiver with PositionReceiver {
  }

}
