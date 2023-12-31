Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / tlBaseVersion := "0.0"

lazy val scala3 = "3.3.1"
ThisBuild / scalaVersion := scala3
ThisBuild / crossScalaVersions := Seq(scala3)

ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / startYear := Some(2023)
ThisBuild / tlSonatypeUseLegacyHost := false

ThisBuild / developers := List(
  tlGitHubDev("buntec", "Christoph Bunte")
)

ThisBuild / tlFatalWarnings := false

lazy val scalajsDomVersion = "2.8.0"
lazy val circeVersion = "0.14.6"
lazy val catsVersion = "2.10.0"
lazy val catsEffectVersion = "3.5.2"
lazy val fs2Version = "3.9.3"
lazy val fs2DomVersion = "0.2.1"
lazy val ff4sVersion = "0.18.0"

lazy val root =
  tlCrossRootProject.aggregate(`ff4s-canvas`, examples)

lazy val `ff4s-canvas` = (project in file("canvas"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := "ff4s-canvas",
    libraryDependencies ++= Seq(
      "io.github.buntec" %%% "ff4s" % ff4sVersion,
      "org.scala-js" %%% "scalajs-dom" % scalajsDomVersion,
      "org.typelevel" %%% "cats-core" % catsVersion,
      "org.typelevel" %%% "cats-free" % catsVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "co.fs2" %%% "fs2-core" % fs2Version,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-literal" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "com.armanbilge" %%% "fs2-dom" % fs2DomVersion
    )
  )

lazy val examples = (project in file("examples"))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core" % "3.2.0",
      "dev.optics" %%% "monocle-macro" % "3.2.0"
    )
  )
  .dependsOn(`ff4s-canvas`)