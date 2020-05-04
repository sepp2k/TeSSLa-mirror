val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val playResolver = "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"
val validatorResolver = "emueller-bintray" at "https://dl.bintray.com/emueller/maven"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

name := "tessla"

organization := "de.uni_luebeck.isp"

val versionFile = new File("src/main/resources/de/uni_luebeck/isp/tessla/stdlib/Tessla.tessla")

val versionPattern = "def\\s+version(?:\\s*:\\s*String)?\\s*=\\s*\"([^\"]+)\"".r

version := versionPattern.findFirstMatchIn(IO.read(versionFile)).get.group(1)

scalaVersion := "2.13.1"

resolvers ++= Seq(
  releases, snapshots,
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
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "com.github.rjeschke" % "txtmark" % "0.13",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "com.typesafe.play" %% "play-json" % "2.8.0",
  "com.eclipsesource" %% "play-json-schema-validator" % "0.9.5" % "test",
  "io.spray" %% "spray-json" % "1.3.5",
  "org.eclipse.tracecompass" % "ctfreader" % "0.2.1-SNAPSHOT",
  "org.typelevel" %% "cats-core" % "2.0.0"
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

assemblyMergeStrategy in assembly := {
  case PathList("module-info.class") => MergeStrategy.discard
  case x => (assemblyMergeStrategy in assembly).value(x)
}
