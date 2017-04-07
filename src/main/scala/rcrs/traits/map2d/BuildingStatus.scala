package rcrs.traits.map2d

import rescuecore2.standard.entities.StandardEntityConstants.Fieryness

case class BuildingStatus(temperature: Int, brokenness: Int, fieryness: Int) extends RCRSNodeStatus
