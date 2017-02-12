package rcrs.scenario

import rcrs.ScalaAgent
import rescuecore2.worldmodel.EntityID
import tcof._
import tcof.traits.map2d.{Map2DTrait, Position}
import rcrs.traits.{WithEntityID, RCRSConnectorTrait}
import rcrs.traits.map2d.RCRSNodeStatus


class RescueScenario(scalaAgent: ScalaAgent) extends Universe with RCRSConnectorTrait with Map2DTrait[RCRSNodeStatus]
  with MobileUnitComponent with CentralUnitComponent with RegistrationSupport with PositionRegistrySupport with AreaExplorationSupport with ObservationSupport {

  this.agent = scalaAgent

//  class PoliceForce(no: Int, _position: Position) extends MobileUnit(_position) {
//    name(s"PoliceForce $no")
//  }

//  class AmbulanceTeam(no: Int, _position: Position) extends MobileUnit(_position) {
//    name(s"AmbulanceTeam $no")
//  }

  class FireBrigade(entityID: EntityID, _position: Position) extends MobileUnit(_position) with WithEntityID {
    val id = entityID
    name(s"FireBrigade $entityID")

    // non-negative value signalizes that brigade needs to refill its tank or is currently refilling the tank
    var amountToRefill: Int = _
    var hydrantToRefill: Position = _
    var assignedFire: Position = _
  }

  class FireStation(entityID: EntityID, _position: Position) extends CentralUnit(_position) with WithEntityID {
    val id = entityID
    name(s"FireStation $entityID")
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

    // TODO - single ensemble for now, but can be divided into different ensembles
    val hydrants: Seq[Position] = ??? // TODO
    val refillEnsemble = ensembles("refill", new RefillCoordination(hydrants))

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


  // Idea: For a given fire, set of fire brigades, hydrants and refugee centers on the map computes
  // the optimal tactics for capturing the fire.
  // Each fire brigade has tank with water which must be replenished once he runs out of water.
  // Water can be replenished either at refugee center (faster, firefighters can replenish in parallel)
  // or at hydrant (slower, only one firefighter at time, but hydrants are usually closer to fire and
  // there are many of them).
  // The ensemble should coordinate fire brigades to minimize damage caused by fire, which usually
  // implies extinguishing the fire as soon as possible and therefore optimal tactics for
  // refilling water is needed.
  //
  // This problem can be (as I think) divided into two subproblems - fire extinguishing + water refilling
  //
  // Fire extinguishing:
  // In simplest case this means pouring as much water as possible (until fire is extinguished)
  // in shortest time, which means minimizing the time spent with travel and refilling (and
  // waiting in queue in case of hydrants).
  //
  // Water refilling:
  // The ensemble coordinates brigades which need to refill (or are currently refilling)
  // the tank. Ensemble computes optimal assignment of brigades to hydrants.
  // Constraints - times spent on single hydrant doesn't overlap
  // TODO - how to encode constraints to utility function?
  //
  // The ensemble coordinates brigades which need to refill the tank
  // Fire brigades can be (I think) assigned to refilling places greedily once they run out of water.
  // The ensemble keeps the schedule when the refilling place is going to be available again.
  //
  // Parameters in simulator with default config values (can be changed):
  // - firefighter tank capacity (7500 in standard rcrs scenario)
  // - hydrant refill rate (150 per cycle)
  // - refugee refill rate (500 per cycle)
  // - firefighter extinguishing rate - amount of water poured to building (500 per cycle)
  //
  class RefillCoordination(val hydrantPositions: Seq[Position]) extends Ensemble {
    name(s"RefillCoordination")

    val toRefill = role("fireBrigadesNeedingRefill", components.select[FireBrigade])
    val hydrantRefillRate = 150

    // does membership and utility condition make sense in this case?
    // members = all firebrigades with flag set
    // utility = function that minimizes travel (+ refill, ...) to available hydrants
    // TODO - optimize globally across all fireBrigades so that even brigades
    // that were previously assigned to hydrant could be rerouted?
    membership(
      // select only brigades that need refill
      toRefill.all{_.amountToRefill > 0}
    )

    val moveSpeed: Int = ???

    def timeToRefillAndReturn(fireBrigade: FireBrigade, hydrant: Position): Int = {
      val timeToReachHydrant = fireBrigade.position.distanceTo(hydrant).toInt / moveSpeed
      val timeFromHydrantToFire = hydrant.distanceTo(fireBrigade.assignedFire).toInt / moveSpeed
      val timeSpentWaitingForHydrant: Int = ???
      val timeSpentRefilling = fireBrigade.amountToRefill / hydrantRefillRate

      timeToReachHydrant + timeSpentWaitingForHydrant + timeFromHydrantToFire + timeSpentRefilling
    }

    utility(
      toRefill.sum(timeToRefillAndReturn(_, ???))
    )

    // actions {
    // }
  }
}

