val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val privateSnapshots = "ISP Private Snapshots" at nexus + "content/repositories/private_snapshots"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val privateReleases = "ISP Private Releases" at nexus + "content/repositories/private_releases"
val releases = "ISP Releases" at nexus + "content/repositories/releases"

name := "tessla"

organization := "de.uni_luebeck.isp"

version := s"0.4.3-SNAPSHOT"

scalaVersion := "2.12.3"

resolvers ++= Seq(
  releases, snapshots,
  privateSnapshots, privateReleases
)

publishTo := {
  if (isSnapshot.value)
    Some(snapshots)
  else
    Some(releases)
}

credentials += Credentials(Path.userHome / ".ivy2" / ".isp-uni-luebeck-maven-repository-credentials")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.sepp2k" %% "sexyopt" % "0.1.1",
  "de.uni_luebeck.isp" %% "compacom" % "0.2.8",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "de.uni_luebeck.isp.tessla.interpreter"
