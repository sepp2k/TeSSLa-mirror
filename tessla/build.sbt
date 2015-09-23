val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "Snapshots" at nexus + "content/repositories/snapshots"
val releases = "Releases" at nexus + "content/repositories/releases"

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
  resolvers += releases,
  resolvers += snapshots,
  libraryDependencies ++= Seq(
    "de.uni_luebeck.isp" %%% "compacom" % "0.2.0-SNAPSHOT",
    "org.json4s" %% "json4s-native" % "3.3.0.RC6"
  ),
  scalacOptions ++= Seq("-feature", "-deprecation", "-target:jvm-1.7")
).jvmSettings(
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.11"  % "test",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )
)

lazy val tesslaJS = tessla.js
lazy val tesslaJVM = tessla.jvm
