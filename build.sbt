ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

publish / skip := true

lazy val rdb = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .in(file("."))
  .settings(
    name := "rdb",
    version := "0.1.0-pre.43",
    scalaVersion := "3.4.0",
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
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.17" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "dal" % "0.1.9",
      "io.github.edadma" %%% "datetime" % "0.1.18",
      "io.github.edadma" %%% "dllist" % "0.1.3",
      "io.github.edadma" %%% "table" % "1.0.4",
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.1.0",
      "com.lihaoyi" %%% "pprint" % "0.8.1" % "test",
    ),
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
  )
  .nativeSettings(
    nativeLinkStubs := true,
    libraryDependencies += "io.github.edadma" %%% "libuuid" % "0.0.1",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.8.1" % "test",
    ),
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "pprint" % "0.8.1" % "test",
    ),
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
//    Test / scalaJSUseMainModuleInitializer := true,
//    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )
