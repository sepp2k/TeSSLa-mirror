lazy val root = (project in file(".")).settings(
  name := "tessla",
  version := "0.1.0-SNAPSHOT",
  scalaVersion := "2.11.4",
  organization := "de.uni_luebeck.isp",
  libraryDependencies += "junit" % "junit" % "4.11",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  scalacOptions ++= Seq("-feature", "-deprecation", "-target:jvm-1.7")
)
