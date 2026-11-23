import org.typelevel.sbt.gha.{PermissionValue, Permissions}

Global / onChangedBuildSource := ReloadOnSourceChanges
Global / resolvers += "Sonatype Central Snapshots" at "https://central.sonatype.com/repository/maven-snapshots/"

ThisBuild / tlBaseVersion := "0.1"

lazy val scala3 = "3.10.1"
ThisBuild / scalaVersion := scala3
ThisBuild / crossScalaVersions := Seq(scala3)

ThisBuild / organization := "io.github.buntec"
ThisBuild / organizationName := "buntec"
ThisBuild / startYear := Some(2023)

ThisBuild / developers := List(
  tlGitHubDev("buntec", "Christoph Bunte")
)

ThisBuild / tlFatalWarnings := false
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"))

val site: TaskKey[File] = taskKey[File]("Generate static site")

// build the examples site and publish it to GitHub Pages on pushes to main
ThisBuild / githubWorkflowAddedJobs += WorkflowJob(
  id = "site",
  name = "Generate Site",
  needs = List("build"),
  cond = Some(
    "github.event_name != 'pull_request' && github.ref == 'refs/heads/main'"
  ),
  javas = List(githubWorkflowJavaVersions.value.head),
  scalas = List(scala3),
  permissions = Some(
    Permissions.Specify.defaultRestrictive
      .withContents(PermissionValue.Write)
  ),
  steps = githubWorkflowJobSetup.value.toList ++ List(
    WorkflowStep.Sbt(List("examples/site"), name = Some("Generate site")),
    WorkflowStep.Use(
      UseRef.Public("peaceiris", "actions-gh-pages", "v4"),
      name = Some("Publish site"),
      params = Map(
        "github_token" -> "${{ secrets.GITHUB_TOKEN }}",
        "publish_dir" -> "examples/target/site",
        "keep_files" -> "true"
      )
    )
  )
)

lazy val scalajsDomVersion = "2.8.1"
lazy val circeVersion = "0.14.17"
lazy val catsVersion = "2.13.0"
lazy val catsEffectVersion = "3.7.1"
lazy val fs2Version = "3.16.0"
lazy val fs2DomVersion = "0.2.1"
lazy val ff4sVersion = "0.27.0"
lazy val monocleVersion = "3.3.0"
lazy val munitVersion = "1.3.6"

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
      "com.armanbilge" %%% "fs2-dom" % fs2DomVersion,
      "org.scalameta" %%% "munit" % munitVersion % Test
    )
  )

lazy val examples = (project in file("examples"))
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    // stable path for examples/index.html, independent of the Scala version
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
      target.value / "fastopt",
    site := {
      val sourceDir1 = (Compile / fullLinkJSOutput).value
      val sourceDir2 = baseDirectory.value / "assets"
      val targetDir = target.value / "site"
      IO.copyDirectory(sourceDir1, targetDir)
      IO.copyDirectory(sourceDir2, targetDir)
      targetDir
    },
    libraryDependencies ++= Seq(
      "dev.optics" %%% "monocle-core" % monocleVersion,
      "dev.optics" %%% "monocle-macro" % monocleVersion
    )
  )
  .dependsOn(`ff4s-canvas`)
