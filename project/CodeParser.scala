import scala.collection.mutable
import scala.util.Try
import scala.collection.immutable
import scala.annotation.tailrec

// This file has no JVM dependencies, so we could even use it on the frontend if needed.
// We would just need to make sure the regex is compatible with JS.
// Note: nested snippets are not supported, in those cases only the outer one will be matched.
object CodeParser {

  class BeginComment(
      val key: String, val lineNumber: Int, val whitespacePrefix: String,
  )

  class EndComment(val key: String, val lineNumber: Int)

  def extractSnippets(
      filePath: String, fileName: String, fileLanguage: String,
      fileContent: String,
      snippetsByKey: mutable.Map[String, mutable.Buffer[SbtCodeSnippet]],
  ): Unit = {
    if (fileLanguage != "css") {
//      println(s"只处理CSS文件：${filePath}")
      return
    }

    val selectors = parseCssSelector(fileContent)
    val codes = selectors.map { case (key, ruler) =>
      SbtCodeSnippet(
          filePath,
          fileName,
          fileLanguage,
          startLineNumber = 0,
          endLineNumber = 0,
          key,
          ruler,
      )
    }

    for (snippet <- codes) {
      val key = snippet.key
      if (snippetsByKey.contains(key)) {
        snippetsByKey(key) += snippet
      } else {
        snippetsByKey.update(key, mutable.Buffer(snippet))
      }
    }
  }

  import com.helger.css.reader.CSSReader
  import java.io.File
  import java.nio.charset.Charset
  import com.helger.css.ECSSVersion
  import scala.jdk.CollectionConverters.*
  import com.helger.css.decl.*
  import scala.annotation.tailrec
  import com.helger.css.ICSSSourceLocationAware
  def extractMember(
      member: ICSSSelectorMember,
  ): List[Either[CSSSelector, String]] = {
    val x = member match {
      case simple: CSSSelectorSimpleMember =>
        Right(simple.getAsCSSString())
      case host: CSSSelectorMemberHost => Left(host.getSelector() :: Nil)
      case content: CSSSelectorMemberHostContext =>
        Left(content.getSelector() :: Nil)
      case slotted: CSSSelectorMemberSlotted =>
        Left(slotted.getSelector() :: Nil)
      case not: CSSSelectorMemberNot =>
        Left(not.getAllSelectors().asScala.toList)
      // case attr: CSSSelectorAttribute =>
      //   if (attr.getAttrValue() != null)
      //     println(attr.getAttrName() -> attr.getAttrValue())
      //   null
      // case fun: CSSSelectorMemberFunctionLike    => null
      // case ecs: ECSSSelectorCombinator           => null
      case other =>
        null
    }
    x match {
      case null         => Nil
      case Left(value)  => value.map(Left.apply)
      case Right(value) => Right(value) :: Nil
    }
  }

  def extractSelector(
      selector: CSSSelector, code: Option[String],
  ): Seq[(String, Option[String])] = {
    selector
      .getAllMembers()
      .asScala
      .flatMap(extractMember)
      .flatMap {
        case Right(value) => value -> code :: Nil
        case Left(value)  => extractSelector(value, None)
      }
      .toSeq
  }

  @tailrec
  def cssClassNames(
      inputList: List[ICSSTopLevelRule], res: List[(String, Seq[String])],
  ): Seq[(String, Seq[String])] = {
    inputList match {
      case head :: tl =>
        head match {
          case other: AbstractHasTopLevelRules =>
            val newInput = other.getAllRules().asScala
            cssClassNames(newInput.toList ::: tl, res)
          case ruler: CSSStyleRule =>
            val code = Option(ruler.getAsCSSString())
            val newValue = ruler
              .getAllSelectors()
              .asScala
              .flatMap(s => extractSelector(s, code))
              .map(x => x.copy(_2 = x._2.toList))
              .toList
            cssClassNames(tl, newValue ::: res)
          case _ => cssClassNames(tl, res)
        }
      case Nil => res
    }
  }

  def parseCssSelector(fileContent: String): Seq[(String, List[String])] = {
    CSSReader
      .readFromString(fileContent, ECSSVersion.LATEST)
      .getAllRules()
      // .getAllStyleRules() //特别需要注意，必须是AllRules，否则会忽略media里面的selector
      .asScala
      .flatMap(ruler => cssClassNames(ruler :: Nil, Nil))
      .filter(_._1.startsWith("."))
      .map(x => (x._1.drop(1), x._2.toList))
      .distinct
  }
}
