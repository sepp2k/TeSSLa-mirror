val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val playResolver = "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"
val validatorResolver = "emueller-bintray" at "https://dl.bintray.com/emueller/maven"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

name := "tessla-compiler"

organization := "de.uni_luebeck.isp"

version := s"0.1.0-SNAPSHOT"

scalaVersion := "2.13.1"

resolvers ++= Seq(
  releases, snapshots,
  playResolver, validatorResolver,
  efficiosSnapshots, efficiosReleases
)

libraryDependencies ++= Seq(
  "de.uni_luebeck.isp" %% "sexyopt" % "0.1.1",
  "de.uni_luebeck.isp" %% "tessla" % "1.2.0-SNAPSHOT",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "com.eclipsesource" %% "play-json-schema-validator" % "0.9.5" % "test"
)

scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

cancelable in Global := true

assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
