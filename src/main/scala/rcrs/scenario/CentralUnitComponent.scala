package rcrs.scenario

import tcof.traits.map2d.Position
import tcof.{Component, Universe}

trait CentralUnitComponent {
  this: Universe with RegistrationSupport with AreaExplorationSupport =>

  abstract class CentralUnit(var position: Position) extends Component
    with PositionAware with AreaExplorationCentral with Registrator {

    // TODO - what should be in this component?
    // - logic for receiving exploration messages is in trait AreaExplorationCentral
    // - registration support (generating short ids) in Registrator
    // - logic for division of components into (sub)ensembles is contained in ensembles
    //   (e.g.) root ensemble
  }

}
