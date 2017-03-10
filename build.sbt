val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val privateSnapshots = "ISP Private Snapshots" at nexus + "content/repositories/private_snapshots"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val privateReleases = "ISP Private Releases" at nexus + "content/repositories/private_releases"
val releases = "ISP Releases" at nexus + "content/repositories/releases"

name := "tessla.interpreter"

organization := "de.uni_luebeck.isp"

version := "0.1.0." + Process("git show -s --format=%ct").lines.head + "-" + Process("git rev-parse HEAD").lines.head + "-SNAPSHOT"

scalaVersion := "2.12.1"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  releases, snapshots,
  privateSnapshots, privateReleases
)

publishTo := {
  if (version.value.trim.endsWith("SNAPSHOT"))
    Some(privateSnapshots)
  else
    Some(privateReleases)
}

credentials += Credentials(Path.userHome / ".ivy2" / ".isp-uni-luebeck-maven-repository-credentials")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2"
)

scalacOptions += "-feature"

scalacOptions += "-deprecation"
