// Resolvers
val nexus = "https://sourcecode.isp.uni-luebeck.de/nexus/"
val snapshots = "ISP Snapshots" at nexus + "content/repositories/snapshots"
val releases = "ISP Releases" at nexus + "content/repositories/releases"
val playResolver = "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"
val validatorResolver = "emueller-bintray" at "https://dl.bintray.com/emueller/maven"
val efficiosSnapshots = "efficios-snapshots" at "https://mvn.efficios.com/repository/snapshots"
val efficiosReleases = "efficios-releases" at "https://mvn.efficios.com/repository/releases"

// Pattern to extract the version from the stdlib file
val versionFile = new File("core/src/main/resources/de/uni_luebeck/isp/tessla/stdlib/Tessla.tessla")
val versionPattern = "def\\s+version(?:\\s*:\\s*String)?\\s*=\\s*\"([^\"]+)\"".r

// Global settings
Global / onChangedBuildSource := ReloadOnSourceChanges
Global / cancelable := true

// ThisBuild scoped settings
val compilerVersion = "2.13.3"
ThisBuild / organization := "de.uni_luebeck.isp"
ThisBuild / scalaVersion := compilerVersion
ThisBuild / version := versionPattern.findFirstMatchIn(IO.read(versionFile)).get.group(1)

ThisBuild / publishTo := { if (isSnapshot.value) Some(snapshots) else Some(releases) }
ThisBuild / credentials += Credentials(
  Path.userHome / ".ivy2" / ".isp-uni-luebeck-maven-repository-credentials"
)

ThisBuild / resolvers ++= Seq(
  releases,
  snapshots,
  playResolver,
  validatorResolver,
  efficiosSnapshots,
  efficiosReleases
)

// Shared dependencies and settings between modules
lazy val commonDependencies = Seq(
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "com.github.rjeschke" % "txtmark" % "0.13",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "com.typesafe.play" %% "play-json" % "2.8.0",
  "com.eclipsesource" %% "play-json-schema-validator" % "0.9.5" % "test",
  "io.spray" %% "spray-json" % "1.3.5",
  "org.eclipse.tracecompass" % "ctfreader" % "0.2.1-SNAPSHOT",
  "org.typelevel" %% "cats-core" % "2.0.0"
)

lazy val commonSettings = Seq(
  libraryDependencies ++= commonDependencies,
  scalacOptions ++= Seq(
    "-feature",
    "-unchecked",
    "-deprecation",
    "-Ypatmat-exhaust-depth",
    "off",
    "-target:jvm-1.8"
  ),
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  assembly / assemblyMergeStrategy := {
    case PathList("module-info.class") => MergeStrategy.discard
    case x                             => (assembly / assemblyMergeStrategy).value(x)
  },
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  Antlr4 / antlr4Version := "4.7.2",
  Antlr4 / antlr4GenListener := false,
  Antlr4 / antlr4GenVisitor := true
)

// Module definitions
val rootPackage = "de.uni_luebeck.isp.tessla"
lazy val root = (project in file("."))
  .enablePlugins(
    BuildInfoPlugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "tessla",
    Compile / run / mainClass := Some(s"$rootPackage.Main"),
    Compile / packageBin / mainClass := Some(s"$rootPackage.Main"),
    buildInfoPackage := rootPackage
  )
  .dependsOn(
    core,
    interpreter,
    docs,
    tesslac
  )
  .aggregate(
    core,
    interpreter,
    docs,
    tesslac
  )

lazy val core = (project in file("core"))
  .enablePlugins(
    BuildInfoPlugin,
    Antlr4Plugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "core",
    Antlr4 / antlr4PackageName := Some(s"$rootPackage.core"),
    buildInfoPackage := s"$rootPackage.core"
  )

lazy val interpreter = (project in file("interpreter"))
  .enablePlugins(
    BuildInfoPlugin,
    Antlr4Plugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "interpreter",
    Antlr4 / antlr4PackageName := Some(s"$rootPackage.interpreter"),
    buildInfoPackage := s"$rootPackage.interpreter"
  )
  .dependsOn(
    core
  )

lazy val docs = (project in file("docs"))
  .enablePlugins(
    BuildInfoPlugin,
    Antlr4Plugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "docs",
    Antlr4 / antlr4PackageName := Some(s"$rootPackage.docs"),
    buildInfoPackage := s"$rootPackage.docs"
  )
  .dependsOn(
    core
  )

lazy val tesslac = (project in file("tessla-compiler"))
  .enablePlugins(
    BuildInfoPlugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "tessla-compiler",
    buildInfoPackage := s"$rootPackage.tessla_compiler",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % compilerVersion
    )
  )
  .dependsOn(
    core
  )
