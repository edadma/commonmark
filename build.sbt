name := "commonmark"

version := "0.1"

scalaVersion := "2.13.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:implicitConversions",
  "-language:existentials"
)

organization := "io.github.edadma"

githubOwner := "edadma"

githubRepository := name.value

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

//libraryDependencies ++= Seq(
//  "com.typesafe" % "config" % "1.3.3"
//)

//libraryDependencies ++= Seq(
//  "jline" % "jline" % "2.14.6"
//)

libraryDependencies ++= Seq(
  "io.github.edadma" %% "dllist" % "0.1.1",
  "io.github.edadma" %% "json" % "0.1.12"
)

mainClass in (Compile, run) := Some(
  "xyz.hyperreal." + name.value.replace('-', '_') + ".Main"
)

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ =>
  false
}

licenses := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))

homepage := Some(url("https://github.com/edadma/" + name.value))

pomExtra :=
  <scm>
    <url>git@github.com:edadma/
      {name.value}
      .git</url>
    <connection>scm:git:git@github.com:edadma/
      {name.value}
      .git</connection>
  </scm>
    <developers>
      <developer>
        <id>edadma</id>
        <name>Edward A. Maxedon, Sr.</name>
      <url>https://github.com/edadma</url>
    </developer>
  </developers>
