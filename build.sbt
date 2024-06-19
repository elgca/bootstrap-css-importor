ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := Versions.Scala_3

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .settings(
      name := "My WebApp",
      publishLocal / skip := true,
      publish / skip := true,
  )
  .settings(commonSettings)
  .settings(
      name := "client",
      libraryDependencies ++= List(
          "com.raquo" %%% "laminar" % Versions.Laminar,
          "com.raquo" %%% "laminar-shoelace" % Versions.LaminarShoelace,
          "com.raquo" %%% "waypoint" % Versions.Waypoint,
          "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core" % Versions.JsoniterScala,
          "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % Versions.JsoniterScala % "compile-internal",
      ),
      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.ESModule)
      },
      scalaJSUseMainModuleInitializer := true,
  )
  .settings(
//      watchSources := watchSources.value.filterNot { source =>
//        source.base.getName.endsWith(".less") || source.base.getName
//          .endsWith(".css") || source.base.getName
//          .endsWith(".scss")
//      },
      (Compile / compile) := ((Compile / compile) dependsOn bootstrap).value,
  )

val bootstrap = taskKey[Unit]("Build Bootstrap CSS Class (frontend)")

bootstrap := {
  CodeGeneratorScalaBootstrap.generate(
      rootPaths = Seq(
          "./node_modules/@tabler/core/dist/css",
          "./style.css",
          "./src/", // watch custom css
      ).map(java.nio.file.Path.of(_)),
      targetPath = java.nio.file.Path
        .of("./src/main/scala/scalabootstrap/"),
      packageName = "scalabootstrap",
      objectName = "scalabootstrap",
      fileFilter = (path: java.nio.file.Path) => {
        if (path.toString.contains("node_modules")) {
          val fileName = path.toFile.getName()
          fileName.endsWith(".min.css")
        } else {
          true
        }
      },
  )
}

// https://youtrack.jetbrains.com/issue/SCL-21839/Intellij-refactor-causes-external-incremental-sbt-compilation-to-fail-consistently
val intellijTargetSettings =
  if (System.getenv("IDEA_INITIAL_DIRECTORY") ne null)
    Seq(
        target := baseDirectory.value / "target-idea",
    )
  else
    Seq.empty

lazy val commonSettings = Seq(
    scalacOptions ++= Seq(
        "-deprecation",
        // "-feature",
        "-language:implicitConversions",
        "-experimental",
    ),
    // idePackagePrefix := Some("elgca.webapp")
) ++ intellijTargetSettings

//
addCommandAlias("cup", ";~fastLinkJS")
