import java.io.{File, FileOutputStream, PrintStream}
import java.nio.file.Path

/**
 * 用来构造bootstrap工具
 */
object CodeGeneratorScalaBootstrap {
  private var maybeLastSnippets: Option[Map[String, List[SbtCodeSnippet]]] =
    None
  def generate(
      rootPaths: Seq[Path], targetPath: Path, packageName: String,
      objectName: String, fileFilter: Path => Boolean,
  ): Unit = {
    import scala.collection.mutable
    val snippetsByKeySeq = rootPaths.map(rootPath =>
      CodeBrowser.findCodeSnippets(rootPath, fileFilter))
    val snippetsByKeyBuf = mutable.Map[String, mutable.Buffer[SbtCodeSnippet]]()
    for {
      map <- snippetsByKeySeq
      (key, snippets) <- map
    } {
      if (snippetsByKeyBuf.contains(key)) {
        val buf = snippetsByKeyBuf(key)
        buf.appendAll(snippets)
      } else {
        val buf = new mutable.ArrayBuffer[SbtCodeSnippet]
        snippetsByKeyBuf.put(key, buf)
        buf.appendAll(snippets)
      }
    }
    val snippetsByKey = snippetsByKeyBuf.mapValues(_.toList).toMap
    // CodeBrowser.findCodeSnippets(rootPath, fileFilter)

    if (!maybeLastSnippets.contains(snippetsByKey)) {
      // println(">>> Generating")
      maybeLastSnippets = Some(snippetsByKey)
      val output = printFile(
          packageName = packageName,
          objectName = objectName,
          snippetsByKey = snippetsByKey,
      )
      writeToFile(
          filePath = targetPath.resolve(objectName + ".scala"),
          fileContent = output,
      )
      println(
          s"\033[1;31m[success] generate css mapping ${targetPath.resolve(objectName + ".scala")}\033[0m",
      )
    } else {
      println("\033[1;31m-- ＣＳＳ　ＦＩＬＥ　ＮＯ ＣＨＡＮＧＥＳ --\033[0m")
    }

  }

  def printFile(
      packageName: String, objectName: String,
      snippetsByKey: Map[String, List[SbtCodeSnippet]],
  ): String = {
    val methods: String = snippetsByKey.keys.toList.sorted
      .map { key =>
        val snippet = snippetsByKey(key)
        val files =
          snippet
            .map(_.filePath)
            .map(new File(_).getName())
            .distinct
            .mkString(";")
        val comment = snippet
          .map(_.lines.mkString("\n"))
          .distinct
          .take(1)
          .flatMap(_.split("\n"))
          .toIterator
          .map(x => "   * " + x)
          .mkString(
              s"/**\n   * Files:${files}\n   * {{{\n",
              "\n",
              "\n   * }}}\n  */",
          )

        (comment, key)
      }
      .flatMap { case (comment, key) =>
        val method = key.replace('-', '_')
        if (method == key) {
          Seq(
              s"""  ${comment}
             |  def ${method}: BtsClass = this
             |""".stripMargin,
          )
        } else {
          Seq(
              // s"""  ${comment}
              //    |  def ${method}: BtsClass = this
              //    |""".stripMargin,
              s"""  ${comment}
             |  def `${key}`: BtsClass = this
             |""".stripMargin,
          )
        }
      }
      .mkString("\n")
    Seq(
        s"package ${packageName}",
        """
        |import scala.quoted.*
        |import scala.annotation.unused
        |import scala.language.implicitConversions
        |
        |extension (inline btsClass: BtsClass)
        |  inline def css: String =
        |    ${ btspImpl('btsClass) }
        |
        |def methodNameToBootstrapClass(rawName: String) = {
        |  val name =
        |    if rawName.startsWith("_") && rawName.charAt(1).isDigit then
        |      rawName.stripPrefix("_")
        |    else rawName
        |  name.replace("_", "-")
        |  // not a good idea
        |  //.replace("per", "/").replace("dot", ".")
        |}
        |
        |def btspImpl(tailwindExpr: Expr[BtsClass])(using Quotes): Expr[String] = {
        |  import quotes.reflect.*
        |
        |  def extractClassNames(
        |    term: Term,
        |    prefix: String = "",
        |    important: Boolean = false
        |  ): List[String] = {
        |    var stack      = List((term, prefix, important))
        |    var classNames = List.empty[String]
        |
        |    while stack.nonEmpty
        |    do
        |      val (currentTerm, currentPrefix, currentImportant) = stack.head
        |      stack = stack.tail
        |
        |      currentTerm match {
        |        case Apply(Select(inner, "important"), List(styles)) =>
        |          stack = (styles, currentPrefix, true) :: stack
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Inlined(_, _, inner) =>
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Select(inner, name) =>
        |          val methodName = methodNameToBootstrapClass(name)
        |          val className =
        |            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}"
        |          classNames = classNames :+ className
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Ident("btsp") =>
        |        // No action needed, just continue processing the remaining stack
        |        case Apply(Ident(name), List(arg)) =>
        |          val methodName = methodNameToBootstrapClass(name)
        |          val className =
        |            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}"
        |          classNames = classNames :+ className
        |          stack = (arg, currentPrefix, currentImportant) :: stack
        |        case Apply(Select(inner, name), List(Literal(StringConstant(value))))
        |            if name == "raw" =>
        |          val className =
        |            s"$currentPrefix${if (currentImportant) "!" else ""}$value"
        |          classNames = classNames :+ className
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Apply(Select(inner, name), List(Literal(StringConstant(opacity))))
        |            if name.endsWith("$") =>
        |          val methodName = methodNameToBootstrapClass(name.stripSuffix("$"))
        |          val className =
        |            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}/${opacity}"
        |          classNames = classNames :+ className
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Apply(Select(inner, name), List(Literal(StringConstant(value)))) =>
        |          val methodName = methodNameToBootstrapClass(name)
        |          val className =
        |            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}[$value]"
        |          classNames = classNames :+ className
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Apply(
        |              Apply(Ident(name), args),
        |              List(Literal(StringConstant(value)))
        |            ) =>
        |          val methodName = methodNameToBootstrapClass(name)
        |          val className =
        |            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}[$value]"
        |          classNames = classNames :+ className
        |          stack =
        |            args.map(arg => (arg, currentPrefix, currentImportant)) ++ stack
        |        case Apply(Select(Ident("btsp"), name), List(inner)) =>
        |          val methodName = methodNameToBootstrapClass(name)
        |          stack =
        |            (inner, s"$currentPrefix${methodName}:", currentImportant) :: stack
        |        case Apply(
        |              Select(inner, "variant"),
        |              List(Literal(StringConstant(selector)), styles)
        |            ) =>
        |          val variantPrefix =
        |            s"$currentPrefix[$selector]:" // Use the selector as provided
        |          val styleClasses = extractClassNames(
        |            styles,
        |            variantPrefix,
        |            currentImportant
        |          ) // Extract classes with the variant prefix
        |          classNames = classNames ++ styleClasses
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case Apply(Select(inner, name), args) =>
        |          val methodName = methodNameToBootstrapClass(name)
        |          val innerClasses = args.flatMap(arg =>
        |            extractClassNames(arg, s"$currentPrefix${methodName}:")
        |          )
        |          classNames = classNames ++ innerClasses
        |          stack = (inner, currentPrefix, currentImportant) :: stack
        |        case unexpectedTerm =>
        |          report.errorAndAbort(s"Unexpected term: $unexpectedTerm")
        |      }
        |
        |    classNames
        |  }
        |
        |  val term            = tailwindExpr.asTerm
        |  val classList       = extractClassNames(term).reverse
        |  val combinedClasses = classList.mkString(" ")
        |  // report.info(s"$combinedClasses")
        |  Expr(combinedClasses)
        |}
        |
        |val btsp = BtsClass()
        |
        |case class BtsClass() {
        |""".stripMargin,
        "",
        methods,
        """
        |  /**
        |  * {{{
        |  *   important this css class 
        |  * }}}
        |  */
        |  def important(@unused styles: BtsClass): BtsClass = this
        |  /**
        |  * {{{
        |       for anything class inject
        |  * }}}
        |  */
        |  def raw(@unused classString: String): BtsClass = this
        |  
        |  /**
        |  * {{{
        |  *    css variant
        |  * }}}
        |  */
        |  def variant(selector: String, styles: BtsClass): BtsClass = this
        |}
        |""".stripMargin,
    ).mkString("\n")
  }

  def writeToFile(filePath: Path, fileContent: String): File = {
    // Fun fact: if you print anything from sbt (specifically, from the
    // client/fastLinkJS task run by scalajs-vite plugin), make sure
    // to add a trailing newline, otherwise vite will break.
    // So, println() - good, print() - bad.
    // println("> WRITE > " + filePath.toString + " (" + fileContent.length + " chars)")
    val outputFile = new File(filePath.toString)
    outputFile.getParentFile.mkdirs()

    val fileOutputStream = new FileOutputStream(outputFile)
    val outputPrintStream = new PrintStream(fileOutputStream)

    outputPrintStream.print(fileContent)
    outputPrintStream.flush()

    // Flush written file contents to disk https://stackoverflow.com/a/4072895/2601788
    fileOutputStream.flush()
    fileOutputStream.getFD.sync()

    outputPrintStream.close()

    outputFile
  }
}
