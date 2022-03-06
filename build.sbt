ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val rdb = crossProject(JSPlatform, JVMPlatform /*, NativePlatform*/ )
  .in(file("."))
  .settings(
    name := "rdb",
    version := "0.1.0-pre.32",
    scalaVersion := "3.1.1",
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
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.11" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "dal" % "0.1.9",
      "io.github.edadma" %%% "datetime" % "0.1.15",
      "io.github.edadma" %%% "dllist" % "0.1.2",
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.1.1",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.0.1",
      "com.lihaoyi" %%% "pprint" % "0.7.1",
    ),
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
//  .nativeSettings(
//    libraryDependencies ++= Seq(
//      "com.github.scopt" %%% "scopt" % "4.0.1"
//    ),
//    nativeLinkStubs := true
//  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.7.1" % "test",
    ),
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )
