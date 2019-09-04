val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

name := "tessla-compiler"

organization := "de.uni_luebeck.isp"

version := s"0.1.0-SNAPSHOT"

scalaVersion := "2.12.3"

resolvers ++= Seq(
  releases, snapshots,
  efficiosSnapshots, efficiosReleases
)

libraryDependencies ++= Seq(
  "com.github.sepp2k" %% "sexyopt" % "0.1.1",
  "de.uni_luebeck.isp" %% "tessla" % "1.0.4-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
)

scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

cancelable in Global := true
