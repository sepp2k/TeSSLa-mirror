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

version := s"0.1.0.${gitTimeStamp}-${gitChecksum}-SNAPSHOT"

scalaVersion := "2.12.1"

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  releases, snapshots,
  privateSnapshots, privateReleases
)

publishTo := {
  if (isSnapshot.value)
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

import scala.xml.{Atom, Text, Elem, Node => XNode, XML}

pomPostProcess := { (node: XNode) =>
  val Elem(prefix, "project", attribs, scope, children@_*) = node
  val replacement = "." + Process("git show -s --format=%ct").lines.head +
    "-" + Process("git rev-parse HEAD").lines.head + "-SNAPSHOT"
  val result = for (child <- children) yield child match {
    case Elem(prefix, "version", attribs, scope, _: Atom[_]) =>
      Elem(prefix, "version", attribs, scope, true, Text(child.text.replace("-SNAPSHOT", replacement)))
    case n => n
  }
  Elem(prefix, "project", attribs, scope, true, result: _*)
}

val checkPublish = taskKey[Unit]("Checks whether all requirements for publication are satisfied.")

checkPublish := {
  if (!version.value.endsWith(s".${gitTimeStamp}-${gitChecksum}${if(isSnapshot.value) "-SNAPSHOT" else ""}")) {
    sys.error("Git information in version is not up to date. Reoad project.")
  }
  if (!gitCommited) {
    sys.error("Git repository has uncommited changes.")
  } 
}

publish := (publish dependsOn checkPublish).value