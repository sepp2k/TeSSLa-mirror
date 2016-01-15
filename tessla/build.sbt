val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "Snapshots" at nexus + "content/repositories/snapshots"
val releases = "Releases" at nexus + "content/repositories/releases"

lazy val tessla = (project in file(".")).settings(
  name := "tessla",
  version := "0.2.0-SNAPSHOT",
  scalaVersion := "2.11.7",
  organization := "de.uni_luebeck.isp",
  resolvers += releases,
  resolvers += snapshots,
  resolvers += "ISP Private" at "https://sourcecode.isp.uni-luebeck.de/nexus/content/groups/private",
  resolvers += "ISP Public" at "https://sourcecode.isp.uni-luebeck.de/nexus/content/groups/public",
  resourceDirectory in Compile <<= baseDirectory(_ / "lib"),
  credentials += Credentials(Path.userHome / ".ivy2" / ".isp-uni-luebeck-maven-repository-credentials"),
  libraryDependencies ++= Seq(
    "de.uni_luebeck.isp" %% "compacom" % "0.2.0-SNAPSHOT",
    "org.json4s" %% "json4s-native" % "3.3.0.RC6",
	"de.uni_luebeck.isp" %% "rltlconv" % "0.1.0-SNAPSHOT"
  ),
  scalacOptions ++= Seq("-feature", "-deprecation", "-target:jvm-1.7"),
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.11"  % "test",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test"
  )
)
