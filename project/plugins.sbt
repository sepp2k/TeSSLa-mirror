resolvers += "ISP Gitlab" at "https://gitlab.isp.uni-luebeck.de/api/v4/projects/483/packages/maven"

logLevel := Level.Warn
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.8.1")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.0")
addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("app.k8ty" % "gitlab-plugin" % "0.0.9-SNAPSHOT")
