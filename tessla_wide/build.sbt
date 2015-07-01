lazy val root = (project in file(".")).
  aggregate(tessla_wideJS, tessla_wideJVM).
  settings(
    publish := {}, publishLocal := {}
  )

val akkaV = "2.3.9"
val sprayV = "1.3.3"

lazy val tessla_wide = (crossProject in file(".")).settings(
  EclipseKeys.useProjectId := true,
  name := "tessla_wide",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.6",
  organization := "de.uni_luebeck.isp",
  libraryDependencies += "de.uni_luebeck.isp" %%% "tessla" % "0.0.1-SNAPSHOT",
  scalacOptions ++= Seq("-feature", "-deprecation", "-target:jvm-1.7")
).jsSettings(
  persistLauncher in Compile := true,
  libraryDependencies += "be.doeraene" %%% "scalajs-jquery" % "0.8.0"
).jvmSettings(
  libraryDependencies ++= Seq(
    "junit" % "junit" % "4.11"  % "test",
    "org.scalatest" %% "scalatest" % "2.2.4" % "test",
    "io.spray"            %%  "spray-can"     % sprayV,
    "io.spray"            %%  "spray-routing" % sprayV,
    "io.spray"            %%  "spray-caching" % sprayV,
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "org.webjars" % "bootstrap" % "3.3.5",
    "org.webjars" % "codemirror" % "5.3",
    "org.rogach" %% "scallop" % "0.9.+"
  ),
  resourceGenerators in Compile <+=
    (resourceManaged in Compile, fullOptJS in Compile in "tessla_wideJS") map {(target, source) =>
      val source_dir = source.data.getParentFile()
      for (ext <- List("opt.js", "opt.js.map", "jsdeps.min.js", "launcher.js")) yield {
        val file_name  = "tessla_wide-" + ext
        val dest = target / "de" / "uni_luebeck" / "isp" / "tessla" / "wide" / "static" / file_name
        IO.copyFile(source_dir / file_name, dest)
        dest
      }
    }
)

lazy val tessla_wideJS: Project = tessla_wide.js
lazy val tessla_wideJVM: Project = tessla_wide.jvm
