name := "rdb"
version := "0.1.0"
scalaVersion := "3.1.1" // 2.13.8"
scalacOptions ++=
  Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics"
  )
organization := "io.github.edadma"
githubOwner := "edadma"
githubRepository := name.value
mainClass := Some(s"${organization.value}.${name.value}.Main")
//    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.11" % "test",
libraryDependencies ++= Seq(
  "io.github.edadma" %% "dal" % "0.1.6"
)
libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.0.1",
  // "com.outr" %% "reactify" % "4.0.7",
  // "com.lihaoyi" %%% "fastparse" % "2.3.3" // waiting for Scala 3 support
  "com.lihaoyi" %% "pprint" % "0.7.1" % "test"
)
publishMavenStyle := true
Test / publishArtifact := false
licenses += "ISC" -> url("https://opensource.org/licenses/ISC")

/*
ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val rdb = crossProject(JSPlatform, JVMPlatform /*, NativePlatform*/ )
  .in(file("."))
  .settings(
    name := "rdb",
    version := "0.1.0",
    scalaVersion := "3.1.1", // 2.13.8",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics"
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
//    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.11" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "cross-platform" % "0.1.3",
      "io.github.edadma" %%% "dal" % "0.1.6"
    ),
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.0.1",
      "com.outr" %%% "reactify" % "4.0.7",
      // "com.lihaoyi" %%% "fastparse" % "2.3.3" // waiting for Scala 3 support
      "com.lihaoyi" %%% "pprint" % "0.7.1" % "test"
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided"
  )
  /*
  .nativeSettings(
    nativeLinkStubs := true
  )
 */
  .jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true
  )
 */
