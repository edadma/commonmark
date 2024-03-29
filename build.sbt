ThisBuild / licenses += "ISC" -> url("https://opensource.org/licenses/ISC")
ThisBuild / versionScheme := Some("semver-spec")

lazy val commonmark = crossProject( /*JSPlatform,*/ JVMPlatform /*, NativePlatform*/ )
  .in(file("."))
  .settings(
    name := "commonmark",
    version := "0.1.0-pre.20",
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
        "-Xasync"
      ),
    organization := "io.github.edadma",
    githubOwner := "edadma",
    githubRepository := name.value,
    mainClass := Some(s"${organization.value}.${name.value}.Main"),
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test",
    libraryDependencies ++= Seq(
      "io.github.edadma" %%% "cross-platform" % "0.1.1",
      "io.github.edadma" %%% "json" % "0.1.12",
      "io.github.edadma" %%% "recognizer" % "0.1.0-pre.11",
      "io.github.edadma" %%% "dllist" % "0.1.1",
      "io.github.edadma" %%% "emoji" % "0.1.0",
      "io.github.edadma" %%% "highlighter" % "0.1.1-pre.8",
    ),
    libraryDependencies ++= Seq(
      "com.github.scopt" %%% "scopt" % "4.0.1",
      "com.lihaoyi" %%% "pprint" % "0.7.3" % "test",
    ),
    publishMavenStyle := true,
    Test / publishArtifact := false,
    licenses += "ISC" -> url("https://opensource.org/licenses/ISC"),
//    fork := true
  )
  .jvmSettings(
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.0.0" % "provided",
  ) /*.
  nativeSettings(
    nativeLinkStubs := true
  )*/ /*.
  jsSettings(
    jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(),
    //    Test / scalaJSUseMainModuleInitializer := true,
    //    Test / scalaJSUseTestModuleInitializer := false,
    Test / scalaJSUseMainModuleInitializer := false,
    Test / scalaJSUseTestModuleInitializer := true,
    scalaJSUseMainModuleInitializer := true,
  )*/
