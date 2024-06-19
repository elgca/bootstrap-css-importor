package elgca.webapp.extops

import scala.quoted.*
import scala.annotation.unused
import scala.language.implicitConversions
import com.raquo.laminar.api.L.{htmlAttr, htmlProp, HtmlAttr, HtmlProp}
import com.raquo.laminar.codecs.StringAsIsCodec

extension (inline btsClass: DynamicAttr)
  inline def _attr: HtmlAttr[String] = htmlAttr(`$$`, StringAsIsCodec)

  inline def `$$` : String =
    ${ dynamicAttrImpl('btsClass) }

def dynamicAttrImpl(
  tailwindExpr: Expr[DynamicAttr],
)(using Quotes): Expr[String] = {
  import quotes.reflect.*

  def extractName(
    term: Term,
    prefix: String = "",
  ): List[String] = {
    var stack      = List((term))
    var classNames = List.empty[String]

    while stack.nonEmpty
    do
      val currentTerm = stack.head
      stack = stack.tail

      currentTerm match {
        case Inlined(_, _, inner) =>
          stack = inner :: stack
        case Ident("dyattr")           =>
        case Ident("DynamicAttr_this") =>
        // No action needed, just continue processing the remaining stack
        case Apply(Select(inner, name), List(Literal(StringConstant(value))))
            if name == "selectDynamic" =>
          val className = value
          classNames = classNames :+ className
          stack = (inner) :: stack
        case unexpectedTerm =>
          report.errorAndAbort(s"Unexpected term: $unexpectedTerm")
      }

    classNames
  }

  val term            = tailwindExpr.asTerm
  val classList       = extractName(term).reverse
  val combinedClasses = classList.mkString("-")
  // report.info(s"$combinedClasses")
  Expr(combinedClasses)
}

val dyattr = DynamicAttr()

import scala.language.dynamics

class DynamicAttr extends scala.Dynamic {
  def selectDynamic(@unused methodName: String): DynamicAttr = this
}
