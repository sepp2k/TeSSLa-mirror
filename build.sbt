val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

name := "tessla-compiler"

organization := "de.uni_luebeck.isp"

version := s"0.1.0-SNAPSHOT"

scalaVersion := "2.13.1"

resolvers ++= Seq(
  releases, snapshots,
  efficiosSnapshots, efficiosReleases
)

libraryDependencies ++= Seq(
  "de.uni_luebeck.isp" %% "tessla" % "1.2.0-SNAPSHOT"
)

scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

cancelable in Global := true
