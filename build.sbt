lazy val root = (project in file(".")).settings(
  name := "tessla",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.4",
  organization := "de.uni_luebeck.isp",
  libraryDependencies += "junit" % "junit" % "4.11",
  libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  libraryDependencies += "de.uni-luebeck.isp" % "compacom_2.11" % "0.0.1-SNAPSHOT",
  scalacOptions ++= Seq("-feature", "-deprecation", "-target:jvm-1.7")
)
