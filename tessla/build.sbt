lazy val root = (project in file(".")).
  aggregate(tesslaJS, tesslaJVM).
  settings(
    publish := {}, publishLocal := {}
  )

lazy val tessla = (crossProject in file(".")).settings(
  EclipseKeys.useProjectId := true,
  name := "tessla",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.6",
  organization := "de.uni_luebeck.isp",
  libraryDependencies += "de.uni_luebeck.isp" %%% "compacom" % "0.1.2-SNAPSHOT",
  scalacOptions ++= Seq("-feature", "-deprecation", "-target:jvm-1.7")
).jvmSettings(
  libraryDependencies += "junit" % "junit" % "4.11"  % "test",
  libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"
)

lazy val tesslaJS = tessla.js
lazy val tesslaJVM = tessla.jvm
