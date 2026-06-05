import xerial.sbt.Sonatype.sonatypeCentralHost

ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / version                := "1.5.2"
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.8.3"
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
    url("https://github.com/edadma/petradb"),
    "scm:git@github.com:edadma/petradb.git",
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

ThisBuild / homepage    := Some(url("https://github.com/edadma/petradb"))
ThisBuild / description := "An embeddable cross-platform SQL database engine for Scala (JVM, JS, Native)"

lazy val commonScalacOptions = Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:dynamics",
)

// ── common: result/value/type types + uPack codecs ──────────────────

lazy val common = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("common"))
  .settings(
    name    := "petradb-common",
    version := "1.5.5",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "io.github.edadma"       %%% "dal"                       % "0.0.10",
      "com.lihaoyi"            %%% "upickle"                   % "4.0.2",
      "io.github.cquiroz"      %%% "scala-java-time"           % "2.6.0",
      "org.scala-lang.modules" %%% "scala-parser-combinators"  % "2.4.0",
      "org.scalatest"          %%% "scalatest"                 % "3.2.19" % Test,
    ),
    publishTo := sonatypePublishToBundle.value,
    Compile / doc / sources := Seq.empty, // Scaladoc NPE in SignatureBuilder — upstream bug
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0",
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js"      %% "scalajs-stubs" % "1.1.0" % "provided",
    libraryDependencies += "io.github.edadma" %%% "libuuid"       % "0.0.1",
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )

// ── engine: the SQL database engine ─────────────────────────────────

lazy val engine = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("engine"))
  .dependsOn(common)
  .settings(
    name    := "petradb-engine",
    version := "1.5.5",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "io.github.edadma"  %%% "dllist"         % "0.0.6",
      "io.github.edadma"  %%% "bptree"         % "0.0.3",
      "io.github.edadma"  %%% "cross_platform" % "0.1.3",
      "io.github.edadma"  %%% "stow"           % "0.0.2",
      "io.github.edadma"  %%% "table"          % "0.0.4",
      "io.github.edadma"  %%% "csv"            % "0.0.3",
      "io.github.edadma"  %%% "importer"       % "0.1.2",
      "com.lihaoyi"       %%% "fastparse"      % "3.1.1",
    ),
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      "com.lihaoyi"   %%% "pprint"   % "0.9.3"  % "test",
    ),
    publishMavenStyle      := true,
    publishTo              := sonatypePublishToBundle.value,
    Test / publishArtifact := false,
    Compile / doc / sources := Seq.empty, // Scaladoc NPE in SignatureBuilder — upstream bug
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js"      %% "scalajs-stubs" % "1.1.0" % "provided",
    libraryDependencies += "io.github.edadma" %%% "libuuid"       % "0.0.1",
    nativeConfig ~= { _.withBuildTarget(scalanative.build.BuildTarget.libraryDynamic) },
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

// ── client: network client ───────────────────────────────────────────

lazy val client = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("client"))
  .dependsOn(common)
  .settings(
    name    := "petradb-client",
    version := "1.5.1",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "fetch"     % "0.0.1",
      "com.lihaoyi"      %%% "upickle"   % "4.0.2",
      "org.scalatest"    %%% "scalatest" % "3.2.19" % Test,
    ),
    publishMavenStyle      := true,
    publishTo              := sonatypePublishToBundle.value,
    Test / publishArtifact := false,
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )

// ── chisel: typed access layer (codecs + repositories) ─────────────

lazy val chisel = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("chisel"))
  .dependsOn(common, engine % Test)
  .settings(
    name    := "petradb-chisel",
    version := "1.5.1",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "dal"       % "0.0.10",
      "org.scalatest"    %%% "scalatest" % "3.2.19" % Test,
    ),
    publishMavenStyle       := true,
    publishTo               := sonatypePublishToBundle.value,
    Test / publishArtifact  := false,
    Compile / doc / sources := Seq.empty, // Scaladoc NPE in SignatureBuilder — upstream bug
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0",
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    // The engine (a Test-only dependency) statically imports Node's `fs`, so the test link needs
    // ES module support. The published artifact depends only on `common` and keeps the default
    // module kind.
    Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule).withSourceMap(false) },
  )
  .nativeSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )

// ── cli: SQL interactive shell ──────────────────────────────────────

lazy val cli = crossProject(JSPlatform, NativePlatform)
  .in(file("cli"))
  .dependsOn(engine, client)
  .settings(
    name    := "petradb-cli",
    version := "1.5.4",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies += "com.lihaoyi" %%% "mainargs" % "0.7.8",
    publish / skip      := true,
    publishLocal / skip := true,
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    scalaJSUseMainModuleInitializer := false,
  )
  .nativeSettings(
    libraryDependencies += "io.github.edadma" %%% "readline" % "0.0.2",
    nativeConfig ~= { _.withBaseName("petradb") },
  )

// ── server: HTTP/JSON API ────────────────────────────────────────────

lazy val server = crossProject(JVMPlatform, JSPlatform)
  .in(file("server"))
  .dependsOn(engine, common)
  .settings(
    name    := "petradb-server",
    version := "1.5.4",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies ++= Seq(
      "com.indoorvivants" %%% "toml"       % "0.3.0",
      "com.lihaoyi"       %%% "upickle"    % "4.0.2",
      "com.lihaoyi"       %%% "mainargs"   % "0.7.8",
      "org.scalatest"     %%% "scalatest"  % "3.2.19" % Test,
    ),
    publish / skip      := true,
    publishLocal / skip := true,
  )
  .jvmSettings(
    libraryDependencies += "io.github.edadma" %% "microserve" % "0.2.0",
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
  )

// ── jdbc: JDBC driver ───────────────────────────────────────────────

lazy val jdbc = project
  .in(file("jdbc"))
  .dependsOn(client.jvm, engine.jvm, server.jvm % Test)
  .settings(
    name    := "petradb-jdbc",
    version := "1.5.4",
    scalacOptions ++= commonScalacOptions,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    assembly / assemblyJarName := "petradb-jdbc.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", _*)             => MergeStrategy.discard
      case _                                    => MergeStrategy.first
    },
    // Publish fat jar: no Scala suffix, no transitive deps
    crossVersion           := CrossVersion.disabled,
    Compile / packageBin   := assembly.value,
    publishMavenStyle      := true,
    publishTo              := sonatypePublishToBundle.value,
    Test / publishArtifact := false,
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      new RuleTransformer(new RewriteRule {
        override def transform(n: Node): Seq[Node] = n match {
          case e: Elem if e.label == "dependencies" => NodeSeq.Empty
          case other                                => other
        }
      }).transform(node).head
    },
  )

// ── integration: end-to-end client + server tests ───────────────────

lazy val integration = crossProject(JVMPlatform, JSPlatform)
  .in(file("integration"))
  .dependsOn(client, server)
  .settings(
    name                := "petradb-integration",
    scalacOptions     ++= commonScalacOptions,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % Test,
    publish / skip      := true,
    publishLocal / skip := true,
  )
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
  )

// ── root aggregate ──────────────────────────────────────────────────

lazy val root = project
  .in(file("."))
  .aggregate(
    common.js, common.jvm, common.native,
    engine.js, engine.jvm, engine.native,
    client.js, client.jvm, client.native,
    chisel.js, chisel.jvm, chisel.native,
    cli.js, cli.native,
    server.jvm, server.js,
    integration.jvm, integration.js,
    jdbc,
  )
  .settings(
    name                := "petradb",
    publish / skip      := true,
    publishLocal / skip := true,
  )
