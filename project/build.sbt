lazy val root = (project in file("."))
  .settings(
    // 编译时依赖关系
    //    - 与常规依赖项不同，这些依赖项在 build.sbt 中可用
    //    - 我们使用 Scala DOM Types 的源代码生成器
    //    在编译时为前端生成代码片段。
    libraryDependencies ++= Seq(
      "com.raquo"    %% "domtypes"     % "17.1.0",
      "net.sf.cssbox" % "jstyleparser" % "4.0.0",
      "com.helger"    % "ph-css"       % "7.0.2",
    ),
    // https://mvnrepository.com/artifact/com.lihaoyi/os-lib
//    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.2"
  )
