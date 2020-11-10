import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import sbt.Keys.artifact

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
Global / coverageEnabled := true
Global / coverageMinimum := 80
Global / coverageFailOnMinimum := true
Global / coverageExcludedPackages := "<empty>;" +
  ".*CLIParser;" +
  ".*Main;" +
  ".*BuildInfo;" +
  ".*HasUniqueIdentifiers;" +
  ".*Tessla;" +
  ".*FlatTessla;" +
  ".*TypedTessla;" +
  ".*TesslaAST;" +
  ".*WithDebugOutput;" +
  ".*Diagnostics;" +
  ".*IntermediateCode;" +
  ".*TesslaCompilerReporter;" +
  ".*ArraySeqMonad;" +
  ".*Lazy;" +
  ".*LazyWithStack;" +
  ".*IncludeResolvers;" +
  ".*CInstrumentation\\.IFullFunDesc;" +
  ".*CInstrumentation\\.IFunDesc;" +
  ".*CInstrumentationBridge;" +
  ".*JavaApi;" +
  ".*Ctf;" +
  ".*CtfEventIterator;" +
  ".*Errors;" +
  ".*Location;" +
  ".*TimeUnit;" +
  ".*TesslaDoc;" +
  ".*Trace;" +
  ".*DocJsonProtocol;"

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

// Module-specific dependencies
lazy val clang = "de.uni_luebeck.isp" %% "clang-instrumentation" % "0.9.2"
lazy val scalac = "org.scala-lang" % "scala-compiler" % compilerVersion
lazy val scopt = "com.github.scopt" %% "scopt" % "4.0.0-RC2"

// Shared dependencies and settings for all modules
lazy val commonDependencies = Seq(
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
  "io.spray" %% "spray-json" % "1.3.5",
  "org.eclipse.tracecompass" % "ctfreader" % "0.2.1-SNAPSHOT",
  "org.typelevel" %% "cats-core" % "2.0.0"
)

// Need to transform to absolute path first to find all files. Perhaps a bug in sbt?
lazy val commonResources = file(file("resources").getAbsolutePath)

lazy val commonSettings = Seq(
  libraryDependencies ++= commonDependencies,
  Test / unmanagedResourceDirectories += commonResources,
  Test / testOptions += Tests.Argument("-oF"),
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
  Antlr4 / antlr4GenVisitor := true,
  headerLicense := Some(
    HeaderLicense.Custom(
      Files.readString(Paths.get("LICENSE-HEADER"), StandardCharsets.UTF_8)
    )
  )
)

// Task to copy the scala-library as managed dependency
// This allows to have it as resource in the final artifact, without having to manually manage it.
// Required by the compilation from tessla to scala
val libCopy = Def
  .task {
    val log = streams.value.log

    val source = (Compile / dependencyClasspath).value
      .find(file =>
        file.get(artifact.key).exists(artifact => artifact.name == "scala-library" && artifact.`type` == "jar")
      )
      .getOrElse {
        log.error("Artifact 'scala-library.jar' could not be found")
        throw new sbt.MessageOnlyException("Failed to copy scala-library")
      }
      .data

    val target = (Compile / resourceManaged).value / "scala-library.jar"

    val cached = FileFunction.cached(
      cacheBaseDirectory = streams.value.cacheDirectory / "scala-library",
      inStyle = FilesInfo.hash,
      outStyle = FilesInfo.exists
    ) { (inputs: Set[File]) =>
      IO.copyFile(inputs.head, target)
      log.info(s"Copied resource")
      log.info(s"$source")
      log.info("  to")
      log.info(s"$target")
      Set(target)
    }

    cached(Set(source)).toSeq
  }
  .triggeredBy(Compile / compile)

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
    buildInfoPackage := rootPackage,
    libraryDependencies ++= Seq(
      scopt
    )
  )
  .dependsOn(
    core % "test->test;compile->compile",
    interpreter,
    docs,
    tesslac,
    instrumenter
  )
  .aggregate(
    core,
    interpreter,
    docs,
    tesslac,
    instrumenter
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
    core % "test->test;compile->compile"
  )

lazy val instrumenter = (project in file("instrumenter"))
  .enablePlugins(
    BuildInfoPlugin,
    Antlr4Plugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "instrumenter",
    Antlr4 / antlr4PackageName := Some(s"$rootPackage.instrumenter"),
    buildInfoPackage := s"$rootPackage.instrumenter",
    libraryDependencies ++= Seq(
      clang
    )
  )
  .dependsOn(
    core % "test->test;compile->compile"
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
    core % "test->test;compile->compile"
  )

lazy val tesslac = (project in file("tessla-compiler"))
  .enablePlugins(
    BuildInfoPlugin
  )
  .settings(commonSettings: _*)
  .settings(
    name := "tessla-compiler",
    buildInfoPackage := s"$rootPackage.tessla_compiler",
    Compile / resourceGenerators += libCopy.taskValue,
    libraryDependencies ++= Seq(
      scalac
    )
  )
  .dependsOn(
    core % "test->test;compile->compile"
  )
