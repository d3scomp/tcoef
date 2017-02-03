package rcrs.scenario

import rcrs.ScalaAgent
import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.RCRSConnectorTrait
import rcrs.traits.map2d.RCRSNodeStatus


class RescueScenario(scalaAgent: ScalaAgent) extends Universe with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus]
  with MobileUnitComponent with CentralUnitComponent with RegistrationSupport with AreaExplorationSupport with ObservationSupport {

  this.agent = scalaAgent

//  class PoliceForce(no: Int, _position: Position) extends MobileUnit(_position) {
//    name(s"PoliceForce $no")
//  }

//  class AmbulanceTeam(no: Int, _position: Position) extends MobileUnit(_position) {
//    name(s"AmbulanceTeam $no")
//  }

  class FireBrigade(no: Int, _position: Position) extends MobileUnit(_position) {
    name(s"FireBrigade $no")
  }

  class FireStation(no: Int, _position: Position) extends CentralUnit(_position) {
    name(s"FireStation $no")
  }

  class ExplorationTeam(val zone: MapZone) extends Ensemble {
    name(s"ExplorationTeam for $zone")

    val mobileUnits = role("mobileUnits", components.select[MobileUnit])

    // implicit conversion, shortens e.g. mobileUnits.cloneEquiv.selectEquiv[FireBrigade]
    // to mobileUnits.selectEquiv[FireBrigade]
    implicit def roleToRoleMembersEquiv(role: Role[RescueScenario.this.MobileUnit]) = role.cloneEquiv

    val fireBrigades = role("fireBrigades", mobileUnits.selectEquiv[FireBrigade])
    //val ambulances = role("ambulanceTeams", mobileUnits.selectEquiv[AmbulanceTeam])
    //val police = role("policeForces", mobileUnits.selectEquiv[PoliceForce])

    membership(
      fireBrigades.cardinality >= 1
      // && ambulances.cardinality >= 1
      // && police.cardinality === 1
    )

    // TODO - use shortest path to zone.center instead of euclidian distance
    def proximityToZoneCenter(unit: MobileUnit) = 100 - (unit.position.distanceTo(zone.center) / 10000).round.toInt

    utility(
      mobileUnits.sum(proximityToZoneCenter(_))
    )

    // TODO - added only to demonstrate functionality in preActions (FP)
    preActions {
      println(s"preActions called on ensemble ${this.toString}")
    }

    actions {
      println(s"action ${this.toStringWithUtility}")

      //mobileUnits.foreachBySelection(_.explorationZone = zone, _.explorationZone = null)
      // assigns zones nulled in System.actions
      mobileUnits.foreachBySelection(_.areaExplorationAssignedZone = zone, _ => ())
    }
  }

  class System extends RootEnsemble {
    val mapZones = for {
      xIdx <- 0 until 1
      yIdx <- 0 until 2
    } yield new MapZone(xIdx, yIdx, 0 /* time - 20 */)

    val explorationTeams = ensembles("explorationTeam", mapZones.map(new ExplorationTeam(_)))

    membership(
      explorationTeams.map(_.fireBrigades).allDisjoint
      // && explorationTeams.map(_.ambulances).allDisjoint
      // && explorationTeams.map(_.police).allDisjoint
    )

    actions {
      // nulls all assigned zones
      // TODO - the information about zones should be contained in ExplorationTeam ensamble
      // this leaks information about zones into parent ensamble
      components.select[MobileUnit].map(_.areaExplorationAssignedZone = null)
    }
  }

  val rootEnsemble = root(new System)

}

