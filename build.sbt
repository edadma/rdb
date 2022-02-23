ThisBuild / scalaVersion := "3.1.1"
ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-language:postfixOps",
    "-language:implicitConversions",
    "-language:existentials",
    "-language:dynamics"
  )
ThisBuild / organization := "io.github.edadma"
ThisBuild / githubOwner := "edadma"
ThisBuild / githubRepository := name.value
ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
//ThisBuild / libraryDependencies ++=
//  Seq(
//    "io.github.edadma" %% "dal" % "0.1.6"
//  )
ThisBuild / mainClass := Some(s"${organization.value}.${name.value}.Main")

lazy val core = project
  .dependsOn(shared)
  .dependsOn(parser)
  .settings(
    name := "rdb",
    version := "0.1.0",
    libraryDependencies ++=
      Seq(
        "io.github.edadma" %% "dal" % "0.1.6"
      ),
    libraryDependencies ++=
      Seq(
        "com.github.scopt" %% "scopt" % "4.0.1",
        // "com.outr" %% "reactify" % "4.0.7",
        ("com.lihaoyi" %% "pprint" % "0.7.1").cross(CrossVersion.for3Use2_13),
        ("com.lihaoyi" %% "fastparse" % "2.3.3").cross(CrossVersion.for3Use2_13),
        ("com.lihaoyi" %% "sourcecode" % "0.2.8").cross(CrossVersion.for3Use2_13)
      )
  )

lazy val shared = project
  .settings(
    libraryDependencies ++=
      Seq(
        "io.github.edadma" %% "dal" % "0.1.6"
      )
  )

lazy val parser = project
  .dependsOn(shared)
  .settings(
    scalaVersion := "2.13.8",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
        "-Ytasty-reader"
      ),
    libraryDependencies ++=
      Seq(
        "com.lihaoyi" %% "fastparse" % "2.3.3",
        "com.lihaoyi" %% "sourcecode" % "0.2.8"
      )
  )

/*
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
  ("com.lihaoyi" %% "fastparse" % "2.3.3").cross(CrossVersion.for3Use2_13),
  ("com.lihaoyi" %% "sourcecode" % "0.2.8").cross(CrossVersion.for3Use2_13),
  ("com.lihaoyi" %% "pprint" % "0.7.1").cross(CrossVersion.for3Use2_13)
)
publishMavenStyle := true
Test / publishArtifact := false
licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
 */

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
