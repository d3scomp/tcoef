package rcrs.scenario

import rcrs.ScalaAgent
import rcrs.comm._
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus
import rescuecore2.worldmodel.EntityID
import tcof._
import tcof.traits.map2d.Map2DTrait

class ProtectScenario(scalaAgent: ScalaAgent) extends Model with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus] {
  this.agent = scalaAgent

  object FireBrigade extends Component {
    val Idle = State
    val Protecting = State
    val Refilling = State
  }

  class FireBrigade(val entityID: EntityID) extends Component {
    def in(state: State): Boolean = states.selectedMembers.exists(_ == state)
    //…

    var protectAroundFire: EntityID = _
    var fireLocation: EntityID = _
  }

  class FireStation(val entityID: EntityID) extends Component {
    val fireCoordination = new FireCoordination(this)
    val fireCoordinationRoot = root(fireCoordination)

    // preActions ...

    actions {
      fireCoordinationRoot.init()

      // TODO - should solve and commit be called here? IMHO yes as sendSpeak sends updated attribute values

      for (protectionTeam <- fireCoordination.protectionTeams.selectedMembers)
        for (brigade <- protectionTeam.brigades.selectedMembers) {
          // coordinator sends attribute values to selected components:
          // - entityID of fire
          // - role of the brigade at fire
          // - state of the component (e.g. Protecting)
          // TODO - how is component switched back to Idle? Is that automatic when component runs
          // out of water or when fire is extinguished ?

          agent.sendSpeak(time, Constants.TO_AGENTS, Message.encode(??? /*TODO*/ ))
        }
      // ...
    }
  }

  class ProtectionTeam(coordinator: FireStation, fireLocation: EntityID) extends Ensemble {

    val brigades = role("brigades",components.select[FireBrigade])

    import FireBrigade.{Idle, Protecting}

    membership(
      // TODO - kde se zjisti uhaseni pozaru? Posilaji agenti svuj changeset?
      // Protecting - stav definovany ve FireBrigade nebo v nejakem traitu?

      brigades.all(brigade => (brigade in Idle) || (brigade in Protecting) && brigade.fireLocation == fireLocation) &&
              brigades.cardinality >= 2 && brigades.cardinality <= 3
    )

    actions {
      for (brigade <- brigades.selectedMembers) {
        brigade.protectAroundFire = fireLocation
        assignRoleAndBuildingsToProtect(brigade)
      }
    }

    def assignRoleAndBuildingsToProtect(brigade: FireBrigade) = ???
  }

  class ExtinguishTeam(coordinator: FireStation, fireLocation: EntityID) extends Ensemble {
    // ...
    val brigades = role("brigades",components.select[FireBrigade])
  }

  class FireCoordination(coordinator: FireStation) extends RootEnsemble /* TODO - will extend just Ensamble */ {
    var buildingsOnFire: Seq[EntityID] = ??? // TODO

    // kazde budove priradi 2-3 agenty, tj. muzu mit spoustu hasicu, kteri nejsou nikam prirazeni
    val extinguishTeams = ensembles(buildingsOnFire.map(new ExtinguishTeam(coordinator, _)))
    val protectionTeams = ensembles(buildingsOnFire.map(new ProtectionTeam(coordinator, _)))

    membership(
      (extinguishTeams.map(_.brigades) ++ protectionTeams.map(_.brigades)).allDisjoint
    )
  }
}