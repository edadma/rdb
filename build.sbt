import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.1"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / sonatypeCredentialHost := sonatypeCentralHost

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal
ThisBuild / resolvers += Resolver.sonatypeCentralSnapshots
ThisBuild / resolvers += Resolver.sonatypeCentralRepo("releases")

ThisBuild / sonatypeProfileName := "io.github.edadma"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/rdb"),
    "scm:git@github.com:edadma/rdb.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage    := Some(url("https://github.com/edadma/rdb"))
ThisBuild / description := "Project description here"

ThisBuild / publishTo := sonatypePublishToBundle.value

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
)

// ── core: the existing rdb library ──────────────────────────────────

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("core"))
  .settings(
    name    := "rdb",
    version := "0.1.2",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "io.github.edadma"  %%% "dal"             % "0.0.10",
      "io.github.edadma"  %%% "dllist"          % "0.0.6",
      "io.github.edadma"  %%% "stow"            % "0.0.1",
      "io.github.edadma"  %%% "table"           % "0.0.3",
      "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
    ),
    libraryDependencies ++= Seq(
      "org.scalatest"          %%% "scalatest"                % "3.2.19" % "test",
      "com.lihaoyi"            %%% "pprint"                   % "0.9.3"  % "test",
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js"      %% "scalajs-stubs" % "1.1.0" % "provided",
    libraryDependencies += "io.github.edadma" %%% "libuuid"       % "0.0.1",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer       := false,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0",
  )

// ── cli: SQL interactive shell ──────────────────────────────────────

lazy val cli = crossProject(JVMPlatform, NativePlatform)
  .in(file("cli"))
  .dependsOn(core)
  .settings(
    name    := "rdb-cli",
    version := "0.0.1",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies += "com.lihaoyi" %%% "mainargs" % "0.7.8",
    publish / skip      := true,
    publishLocal / skip := true,
  )
  .jvmSettings(
    libraryDependencies += "org.jline" % "jline" % "3.29.0",
  )
  .nativeSettings(
    libraryDependencies += "io.github.edadma" %%% "readline" % "0.0.2",
  )

// ── root aggregate ──────────────────────────────────────────────────

lazy val root = project
  .in(file("."))
  .aggregate(
    core.js, core.jvm, core.native,
    cli.jvm, cli.native,
  )
  .settings(
    name                := "rdb-root",
    publish / skip      := true,
    publishLocal / skip := true,
  )
