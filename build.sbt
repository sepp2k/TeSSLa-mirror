import com.typesafe.sbt.SbtStartScript

val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "Snapshots" at nexus + "content/repositories/snapshots"
val releases = "Releases" at nexus + "content/repositories/releases"

lazy val tessla = (project in file(".")).settings(
//  offline := true,
  name := "tessla",
  version := "0.3.3-SNAPSHOT",
  scalaVersion := "2.12.1",
  organization := "de.uni_luebeck.isp",
  resolvers += releases,
  resolvers += snapshots,
  resolvers += "ISP Public" at nexus + "content/groups/public",
  resolvers += Resolver.sonatypeRepo("public"),
  resourceDirectory in Compile <<= baseDirectory(_ / "lib"),
  libraryDependencies ++= Seq(
    "de.uni_luebeck.isp" %% "compacom" % "0.2.6",
    "com.github.scopt" %% "scopt" % "3.5.0"
  ),
  scalacOptions ++= Seq("-feature", "-deprecation"),
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.11"  % "test",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  ),
  // disable publishing the main sources jar
  publishArtifact in (Compile, packageSrc) := false,
  publishTo <<= version { (v: String) =>
    if (v.trim.endsWith("SNAPSHOT"))
      Some(snapshots)
    else
      Some(releases)
  },
  credentials += Credentials(Path.userHome / ".ivy2" / ".isp-uni-luebeck-maven-repository-credentials"),
  SbtStartScript.startScriptForClassesSettings
)
