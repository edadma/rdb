ThisBuild / licenses               := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme          := Some("semver-spec")
ThisBuild / evictionErrorLevel     := Level.Warn
ThisBuild / scalaVersion           := "3.7.2"
ThisBuild / organization           := "io.github.edadma"
ThisBuild / organizationName       := "edadma"
ThisBuild / organizationHomepage   := Some(url("https://github.com/edadma"))
ThisBuild / version                := "0.0.7"
ThisBuild / sonatypeCredentialHost := "central.sonatype.com"

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

ThisBuild / homepage := Some(url("https://github.com/edadma/rdb"))

ThisBuild / publishTo := {
  val centralSnapshots = "https://central.sonatype.com/repository/maven-snapshots/"
  if (isSnapshot.value) Some("central-snapshots" at centralSnapshots)
  else localStaging.value
}

lazy val rdb = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "rdb",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "dal"      % "0.0.2",
      "io.github.edadma" %%% "datetime" % "0.0.1",
      "io.github.edadma" %%% "dllist"   % "0.0.6",
      "io.github.edadma" %%% "table"    % "0.0.1",
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
    //  scalaJSLinkerConfig ~= { _.withModuleSplitStyle(ModuleSplitStyle.SmallestModules) },
    scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer      := false,
    Test / scalaJSUseTestModuleInitializer      := true,
    scalaJSUseMainModuleInitializer             := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0",
  )

lazy val root = project
  .in(file("."))
  .aggregate(rdb.js, rdb.jvm, rdb.native)
  .settings(
    name                := "rdb",
    publish / skip      := true,
    publishLocal / skip := true,
  )
