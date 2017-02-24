package rcrs.traits.map2d

import java.io._

import tcof.traits.map2d.Position
import rcrs.LineOfSight
import rescuecore2.config.Config
import rescuecore2.standard.entities.{Area, StandardWorldModel}
import rescuecore2.worldmodel.EntityID

import scala.collection.JavaConverters._
import scala.collection.mutable


object RCRSMapStatic {
  private var initialized = false

  val lineOfSight = mutable.Map.empty[EntityID, Set[EntityID]]

  case class CloseAreaIDs(byIdx: Map[Int, EntityID], byAreaId: Map[EntityID, Int])
  val closeAreaIDs = mutable.Map.empty[EntityID, CloseAreaIDs]

  private def computeLineOfSight(config: Config, model: StandardWorldModel): Unit = {
    println("Computation of lines of sight...")

    val modelIterable = model.asScala

    val entityCount = modelIterable.size
    var entityIndex = 0

    for (entity <- modelIterable) {
      entity match {
        case area: Area =>
          val los = new LineOfSight(config, model)
          val visibleEntities = los.getVisibleEntities(Position(area.getX, area.getY)).asScala.collect{ case visibleArea: Area => visibleArea.getID }.toSet
          lineOfSight += (area.getID -> visibleEntities)
        case _ =>
      }

      entityIndex = entityIndex + 1

      if (entityIndex % 100 == 0) {
        println(s"  $entityIndex / $entityCount")
      }
    }
  }

  private def computeCloseAreaIDs(config: Config, model: StandardWorldModel): Unit = {
    println("Computation of short area-based indexes...")

    val areas = model.asScala.collect{ case area: Area => area }.toList

    for (area <- areas) {
      val areaPos = Position(area.getX, area.getY)
      val cAreas = areas.sortBy(a => areaPos.distanceTo(Position(a.getX, a.getY))).map(_.getID).zipWithIndex
      closeAreaIDs += area.getID -> CloseAreaIDs(cAreas.map(_.swap).toMap, cAreas.toMap)
    }
  }

  def initialize(config: Config, model: StandardWorldModel): Unit = {
    val precomputeFileName = "precompute.data"

    if (!initialized) {
      // try to load map from file
      var input: ObjectInputStream = null
      try {
        input = new ObjectInputStream(new FileInputStream(precomputeFileName))

        lineOfSight.clear()
        lineOfSight ++= input.readObject().asInstanceOf[mutable.Map[Int, Set[Int]]]
          .map{ case (key, value) => new EntityID(key) -> value.map(valueID => new EntityID(valueID)) }  // transforms Map[Int, Set[Int]] to Map[EntityID, Set[EntityID]]

        closeAreaIDs.clear()
        closeAreaIDs ++= input.readObject().asInstanceOf[mutable.Map[Int, (Map[Int,Int], Map[Int,Int])]]
          .map{ case (refAreaId, (byIdx, byAreaId)) => new EntityID(refAreaId) -> CloseAreaIDs(byIdx.mapValues(new EntityID(_)), byAreaId.map{ case (id, idx) => new EntityID(id) -> idx }) }

        println(s"Loaded precomputed data from '${precomputeFileName}', size: ${closeAreaIDs.size}")
        initialized = true
      } catch {
        case e: FileNotFoundException => println(s"File with precomputed data '${precomputeFileName}' not found")
      } finally {
        if (input != null) {
          input.close()
        }
      }
    }

    if (!initialized) {
      computeLineOfSight(config, model)
      computeCloseAreaIDs(config, model)

      // try to save map to file
      var output: ObjectOutputStream = null
      try {
        val output = new ObjectOutputStream(new FileOutputStream(precomputeFileName))

        output.writeObject(lineOfSight.map{ case (key, value) => (key.getValue, value.map(_.getValue)) })  // transforms Map[EntityID, Set[EntityID]] to Map[Int, Set[Int]]

        output.writeObject(closeAreaIDs.map{
          case (refAreaId, CloseAreaIDs(byIdx, byAreaId)) =>
            refAreaId.getValue -> (byIdx.mapValues(_.getValue).map(identity), byAreaId.map{ case (id, idx) => id.getValue -> idx })
        })

        println(s"Saved precomputed data to \'${precomputeFileName}\'")
      } finally {
        if (output != null) {
          output.close
        }
      }

      println(s"  finished")
      initialized = true
    }
  }

}
