val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val privateSnapshots = "ISP Private Snapshots" at nexus + "content/repositories/private_snapshots"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val privateReleases = "ISP Private Releases" at nexus + "content/repositories/private_releases"
val releases = "ISP Releases" at nexus + "content/repositories/releases"

name := "tessla.interpreter"

organization := "de.uni_luebeck.isp"

def gitTimeStamp = Process("git show -s --format=%ct").lines.head
def gitChecksum = Process("git rev-parse HEAD").lines.head
def gitCommited = Process("git status --porcelain").lines.isEmpty

version := s"0.1.1.${gitTimeStamp}-${gitChecksum}-SNAPSHOT"

scalaVersion := "2.12.1"

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
  "de.uni_luebeck.isp" %% "tessla" % "0.3.7-SNAPSHOT",
  "com.chuusai" %% "shapeless" % "2.3.2",
  "com.github.sepp2k" %% "sexyopt" % "0.1.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"

val checkPublish = taskKey[Unit]("Checks whether all requirements for publication are satisfied.")

checkPublish := {
  streams.value.log.info("asd")
  if (!version.value.endsWith(s".${gitTimeStamp}-${gitChecksum}${if(isSnapshot.value) "-SNAPSHOT" else ""}")) {
    throw new Incomplete(None, message= Some("Git information in version is not up to date. Reoad project."))
  }
  if (!gitCommited) {
    throw new Incomplete(None, message= Some("Git repository has uncommited changes."))
  }
}

publish := (publish dependsOn checkPublish).value

enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "de.uni_luebeck.isp.tessla.interpreter"
