name := "tcof"

version := "1.0"

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(

  // Required for mpmens
  "org.scala-lang" % "scala-reflect" % "2.12.0",
  "org.choco-solver" % "choco-solver" % "4.0.0",

  // Required for map2d trait
  "de.ummels" %% "scala-prioritymap" % "1.0.0",

  // Required for statespace and statistics traits
  "org.apache.commons" % "commons-math3" % "3.6.1",

  // Required for RoboRescue
  "org.apache.logging.log4j" % "log4j-api" % "2.7",
  "org.apache.logging.log4j" % "log4j-core" % "2.7",
  "org.apache.logging.log4j" % "log4j-1.2-api" % "2.7",
  "org.uncommons.maths" % "uncommons-maths" % "1.2.2a",
  "trove" % "trove" % "1.0.2",
  "org.scala-lang" % "scala-library" % "2.12.0",
  "org.scodec" %% "scodec-core" % "1.10.3",
  "org.scodec" %% "scodec-bits" % "1.1.2"

  // Unit tests
  // "org.scalactic" %% "scalactic" % "3.0.1",
  // "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

cleanFiles += (baseDirectory(_ / "precompute.data")).value
