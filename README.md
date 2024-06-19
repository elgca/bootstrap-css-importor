# bootstrap-css-importor
This demo is used to demonstrate how to incorporate Bootstrap CSS into ScalaJS, similar to the Scalawind project

## CSS To Scala Generator

This is a pre compiled generator built in SBT,
which runs before each compilation of code to generate
[scalabootstrap.scala](src/main/scala/scalabootstrap/scalabootstrap.scala)。

Or manually start it by:
```shell
sbt bootstrap
```

The template of scalabootstrap.scala comes from Scalawind。

## Any CSS Supported

Just modify the configuration directory in build.sbt to add any CSS to the generator,
and any CSS can be added to it

```scala
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
```