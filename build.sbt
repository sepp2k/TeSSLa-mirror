val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val privateSnapshots = "ISP Private Snapshots" at nexus + "content/repositories/private_snapshots"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val privateReleases = "ISP Private Releases" at nexus + "content/repositories/private_releases"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val playResolver = "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
val validatorResolver = "emueller-bintray" at "http://dl.bintray.com/emueller/maven"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

name := "tessla"

organization := "de.uni_luebeck.isp"

val versionFile = new File("src/main/resources/de/uni_luebeck/isp/tessla/stdlib/Tessla.tessla")

val versionPattern = "def\\s+version(?:\\s*:\\s*String)?\\s*=\\s*\"([^\"]+)\"".r

version := versionPattern.findFirstMatchIn(IO.read(versionFile)).get.group(1)

scalaVersion := "2.12.7"

resolvers ++= Seq(
  releases, snapshots,
  privateSnapshots, privateReleases,
  playResolver, validatorResolver,
  efficiosSnapshots, efficiosReleases
)

publishTo := {
  if (isSnapshot.value)
    Some(snapshots)
  else
    Some(releases)
}

credentials += Credentials(Path.userHome / ".ivy2" / ".isp-uni-luebeck-maven-repository-credentials")

libraryDependencies ++= Seq(
  "com.github.sepp2k" %% "sexyopt" % "0.1.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe.play" %% "play-json" % "2.6.6" % "test",
  "com.eclipsesource" %% "play-json-schema-validator" % "0.9.4" % "test",
  "org.eclipse.tracecompass" % "ctfreader" % "0.2.1-SNAPSHOT"
)

mainClass in (Compile, run) := Some("de.uni_luebeck.isp.tessla.Main")
mainClass in (Compile, packageBin) := Some("de.uni_luebeck.isp.tessla.Main")

scalacOptions += "-feature"
scalacOptions += "-unchecked"
scalacOptions += "-deprecation"
scalacOptions ++= Seq("-Ypatmat-exhaust-depth", "off")

cancelable in Global := true

enablePlugins(BuildInfoPlugin)
buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion)
buildInfoPackage := "de.uni_luebeck.isp.tessla"

enablePlugins(Antlr4Plugin)
antlr4Version in Antlr4 := "4.7.2"
antlr4PackageName in Antlr4 := Some("de.uni_luebeck.isp.tessla")
antlr4GenListener in Antlr4 := false
antlr4GenVisitor in Antlr4 := true

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
scalacOptions += "-target:jvm-1.8"
