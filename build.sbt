val dottyVersion = "0.23.0-RC1"

lazy val core = project
  .in(file("core"))
  .settings(
    name := "Monocly",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.2" % "test",
    testFrameworks += new TestFramework("munit.Framework")
  )

lazy val benchmarks =
  project
    .in(file("benchmarks"))
    .dependsOn(core)
    .settings(
      name := "benchmarks",
      scalaVersion := dottyVersion,
      javaOptions += "-XX:MaxInlineLevel=21",
    )
    .enablePlugins(JmhPlugin)
