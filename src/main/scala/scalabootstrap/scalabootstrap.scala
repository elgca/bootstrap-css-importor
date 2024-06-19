package scalabootstrap

import scala.quoted.*
import scala.annotation.unused
import scala.language.implicitConversions

extension (inline btsClass: BtsClass)
  inline def css: String =
    ${ btspImpl('btsClass) }

def methodNameToBootstrapClass(rawName: String) = {
  val name =
    if rawName.startsWith("_") && rawName.charAt(1).isDigit then
      rawName.stripPrefix("_")
    else rawName
  name.replace("_", "-")
  // not a good idea
  //.replace("per", "/").replace("dot", ".")
}

def btspImpl(tailwindExpr: Expr[BtsClass])(using Quotes): Expr[String] = {
  import quotes.reflect.*

  def extractClassNames(
    term: Term,
    prefix: String = "",
    important: Boolean = false
  ): List[String] = {
    var stack      = List((term, prefix, important))
    var classNames = List.empty[String]

    while stack.nonEmpty
    do
      val (currentTerm, currentPrefix, currentImportant) = stack.head
      stack = stack.tail

      currentTerm match {
        case Apply(Select(inner, "important"), List(styles)) =>
          stack = (styles, currentPrefix, true) :: stack
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Inlined(_, _, inner) =>
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Select(inner, name) =>
          val methodName = methodNameToBootstrapClass(name)
          val className =
            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}"
          classNames = classNames :+ className
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Ident("btsp") =>
        // No action needed, just continue processing the remaining stack
        case Apply(Ident(name), List(arg)) =>
          val methodName = methodNameToBootstrapClass(name)
          val className =
            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}"
          classNames = classNames :+ className
          stack = (arg, currentPrefix, currentImportant) :: stack
        case Apply(Select(inner, name), List(Literal(StringConstant(value))))
            if name == "raw" =>
          val className =
            s"$currentPrefix${if (currentImportant) "!" else ""}$value"
          classNames = classNames :+ className
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Apply(Select(inner, name), List(Literal(StringConstant(opacity))))
            if name.endsWith("$") =>
          val methodName = methodNameToBootstrapClass(name.stripSuffix("$"))
          val className =
            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}/${opacity}"
          classNames = classNames :+ className
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Apply(Select(inner, name), List(Literal(StringConstant(value)))) =>
          val methodName = methodNameToBootstrapClass(name)
          val className =
            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}[$value]"
          classNames = classNames :+ className
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Apply(
              Apply(Ident(name), args),
              List(Literal(StringConstant(value)))
            ) =>
          val methodName = methodNameToBootstrapClass(name)
          val className =
            s"$currentPrefix${if (currentImportant) "!" else ""}${methodName}[$value]"
          classNames = classNames :+ className
          stack =
            args.map(arg => (arg, currentPrefix, currentImportant)) ++ stack
        case Apply(Select(Ident("btsp"), name), List(inner)) =>
          val methodName = methodNameToBootstrapClass(name)
          stack =
            (inner, s"$currentPrefix${methodName}:", currentImportant) :: stack
        case Apply(
              Select(inner, "variant"),
              List(Literal(StringConstant(selector)), styles)
            ) =>
          val variantPrefix =
            s"$currentPrefix[$selector]:" // Use the selector as provided
          val styleClasses = extractClassNames(
            styles,
            variantPrefix,
            currentImportant
          ) // Extract classes with the variant prefix
          classNames = classNames ++ styleClasses
          stack = (inner, currentPrefix, currentImportant) :: stack
        case Apply(Select(inner, name), args) =>
          val methodName = methodNameToBootstrapClass(name)
          val innerClasses = args.flatMap(arg =>
            extractClassNames(arg, s"$currentPrefix${methodName}:")
          )
          classNames = classNames ++ innerClasses
          stack = (inner, currentPrefix, currentImportant) :: stack
        case unexpectedTerm =>
          report.errorAndAbort(s"Unexpected term: $unexpectedTerm")
      }

    classNames
  }

  val term            = tailwindExpr.asTerm
  val classList       = extractClassNames(term).reverse
  val combinedClasses = classList.mkString(" ")
  // report.info(s"$combinedClasses")
  Expr(combinedClasses)
}

val btsp = BtsClass()

case class BtsClass() {


  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion {
   *   --tblr-accordion-color:var(--tblr-body-color);
   *   --tblr-accordion-bg:transparent;
   *   --tblr-accordion-transition:color 0.15s ease-in-out,background-color 0.15s ease-in-out,border-color 0.15s ease-in-out,box-shadow 0.15s ease-in-out,border-radius 0.15s ease;
   *   --tblr-accordion-border-color:var(--tblr-border-color-translucent);
   *   --tblr-accordion-border-width:var(--tblr-border-width);
   *   --tblr-accordion-border-radius:var(--tblr-border-radius);
   *   --tblr-accordion-inner-border-radius:calc(var(--tblr-border-radius) - (var(--tblr-border-width)));
   *   --tblr-accordion-btn-padding-x:1.25rem;
   *   --tblr-accordion-btn-padding-y:1rem;
   *   --tblr-accordion-btn-color:var(--tblr-body-color);
   *   --tblr-accordion-btn-bg:transparent;
   *   --tblr-accordion-btn-icon:url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%23182433'%3e%3cpath fill-rule='evenodd' d='M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z'/%3e%3c/svg%3e");
   *   --tblr-accordion-btn-icon-width:1rem;
   *   --tblr-accordion-btn-icon-transform:rotate(-180deg);
   *   --tblr-accordion-btn-icon-transition:transform 0.2s ease-in-out;
   *   --tblr-accordion-btn-active-icon:url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' fill='%23002242'%3e%3cpath fill-rule='evenodd' d='M1.646 4.646a.5.5 0 0 1 .708 0L8 10.293l5.646-5.647a.5.5 0 0 1 .708.708l-6 6a.5.5 0 0 1-.708 0l-6-6a.5.5 0 0 1 0-.708z'/%3e%3c/svg%3e");
   *   --tblr-accordion-btn-focus-border-color:var(--tblr-border-color-translucent);
   *   --tblr-accordion-btn-focus-box-shadow:0 0 0 0.25rem rgba(var(--tblr-primary-rgb),0.25);
   *   --tblr-accordion-body-padding-x:1.25rem;
   *   --tblr-accordion-body-padding-y:1rem;
   *   --tblr-accordion-active-color:inherit;
   *   --tblr-accordion-active-bg:transparent;
   * }
   * }}}
  */
  def accordion: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion-body { padding:var(--tblr-accordion-body-padding-y) var(--tblr-accordion-body-padding-x); }
   * }}}
  */
  def `accordion-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion-button {
   *   position:relative;
   *   display:flex;
   *   align-items:center;
   *   width:100%;
   *   padding:var(--tblr-accordion-btn-padding-y) var(--tblr-accordion-btn-padding-x);
   *   font-size:.875rem;
   *   color:var(--tblr-accordion-btn-color);
   *   text-align:left;
   *   background-color:var(--tblr-accordion-btn-bg);
   *   border:0;
   *   border-radius:0;
   *   overflow-anchor:none;
   *   transition:var(--tblr-accordion-transition);
   * }
   * }}}
  */
  def `accordion-button`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion-item:last-of-type .accordion-collapse {
   *   border-bottom-right-radius:var(--tblr-accordion-border-radius);
   *   border-bottom-left-radius:var(--tblr-accordion-border-radius);
   * }
   * }}}
  */
  def `accordion-collapse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion-flush .accordion-collapse { border-width:0; }
   * }}}
  */
  def `accordion-flush`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion-header { margin-bottom:0; }
   * }}}
  */
  def `accordion-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .accordion-item {
   *   color:var(--tblr-accordion-color);
   *   background-color:var(--tblr-accordion-bg);
   *   border:var(--tblr-accordion-border-width) solid var(--tblr-accordion-border-color);
   * }
   * }}}
  */
  def `accordion-item`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-remove_button:not(.rtl) .item.active .remove { border-left-color:transparent; }
   * }}}
  */
  def active: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert {
   *   --tblr-alert-bg:transparent;
   *   --tblr-alert-padding-x:1rem;
   *   --tblr-alert-padding-y:0.75rem;
   *   --tblr-alert-margin-bottom:1rem;
   *   --tblr-alert-color:inherit;
   *   --tblr-alert-border-color:transparent;
   *   --tblr-alert-border:var(--tblr-border-width) solid var(--tblr-alert-border-color);
   *   --tblr-alert-border-radius:var(--tblr-border-radius);
   *   --tblr-alert-link-color:inherit;
   *   position:relative;
   *   padding:var(--tblr-alert-padding-y) var(--tblr-alert-padding-x);
   *   margin-bottom:var(--tblr-alert-margin-bottom);
   *   color:var(--tblr-alert-color);
   *   background-color:var(--tblr-alert-bg);
   *   border:var(--tblr-alert-border);
   *   border-radius:var(--tblr-alert-border-radius);
   * }
   * }}}
  */
  def alert: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-azure {
   *   --tblr-alert-color:var(--tblr-azure-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-azure-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-azure-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-azure-text-emphasis);
   * }
   * }}}
  */
  def `alert-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-bitbucket {
   *   --tblr-alert-color:var(--tblr-bitbucket-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-bitbucket-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-bitbucket-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-bitbucket-text-emphasis);
   * }
   * }}}
  */
  def `alert-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-blue {
   *   --tblr-alert-color:var(--tblr-blue-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-blue-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-blue-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-blue-text-emphasis);
   * }
   * }}}
  */
  def `alert-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-cyan {
   *   --tblr-alert-color:var(--tblr-cyan-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-cyan-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-cyan-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-cyan-text-emphasis);
   * }
   * }}}
  */
  def `alert-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-danger {
   *   --tblr-alert-color:var(--tblr-danger-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-danger-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-danger-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-danger-text-emphasis);
   * }
   * }}}
  */
  def `alert-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-dark {
   *   --tblr-alert-color:var(--tblr-dark-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-dark-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-dark-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-dark-text-emphasis);
   * }
   * }}}
  */
  def `alert-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-dismissible { padding-right:3rem; }
   * }}}
  */
  def `alert-dismissible`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-dribbble {
   *   --tblr-alert-color:var(--tblr-dribbble-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-dribbble-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-dribbble-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-dribbble-text-emphasis);
   * }
   * }}}
  */
  def `alert-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-facebook {
   *   --tblr-alert-color:var(--tblr-facebook-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-facebook-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-facebook-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-facebook-text-emphasis);
   * }
   * }}}
  */
  def `alert-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-flickr {
   *   --tblr-alert-color:var(--tblr-flickr-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-flickr-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-flickr-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-flickr-text-emphasis);
   * }
   * }}}
  */
  def `alert-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-github {
   *   --tblr-alert-color:var(--tblr-github-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-github-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-github-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-github-text-emphasis);
   * }
   * }}}
  */
  def `alert-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-google {
   *   --tblr-alert-color:var(--tblr-google-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-google-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-google-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-google-text-emphasis);
   * }
   * }}}
  */
  def `alert-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-green {
   *   --tblr-alert-color:var(--tblr-green-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-green-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-green-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-green-text-emphasis);
   * }
   * }}}
  */
  def `alert-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-heading { color:inherit; }
   * }}}
  */
  def `alert-heading`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-important .alert-icon,
   * .alert-important .alert-link,
   * .alert-important .alert-title { color:inherit; }
   * }}}
  */
  def `alert-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-important {
   *   border-color:transparent;
   *   background:var(--tblr-alert-color);
   *   color:#fff;
   * }
   * }}}
  */
  def `alert-important`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-indigo {
   *   --tblr-alert-color:var(--tblr-indigo-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-indigo-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-indigo-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-indigo-text-emphasis);
   * }
   * }}}
  */
  def `alert-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-info {
   *   --tblr-alert-color:var(--tblr-info-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-info-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-info-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-info-text-emphasis);
   * }
   * }}}
  */
  def `alert-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-instagram {
   *   --tblr-alert-color:var(--tblr-instagram-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-instagram-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-instagram-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-instagram-text-emphasis);
   * }
   * }}}
  */
  def `alert-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-light {
   *   --tblr-alert-color:var(--tblr-light-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-light-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-light-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-light-text-emphasis);
   * }
   * }}}
  */
  def `alert-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-lime {
   *   --tblr-alert-color:var(--tblr-lime-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-lime-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-lime-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-lime-text-emphasis);
   * }
   * }}}
  */
  def `alert-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-link {
   *   font-weight:var(--tblr-font-weight-bold);
   *   color:var(--tblr-alert-link-color);
   * }
   * }}}
  */
  def `alert-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-linkedin {
   *   --tblr-alert-color:var(--tblr-linkedin-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-linkedin-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-linkedin-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-linkedin-text-emphasis);
   * }
   * }}}
  */
  def `alert-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-muted {
   *   --tblr-alert-color:var(--tblr-muted-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-muted-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-muted-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-muted-text-emphasis);
   * }
   * }}}
  */
  def `alert-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-orange {
   *   --tblr-alert-color:var(--tblr-orange-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-orange-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-orange-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-orange-text-emphasis);
   * }
   * }}}
  */
  def `alert-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-pink {
   *   --tblr-alert-color:var(--tblr-pink-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-pink-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-pink-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-pink-text-emphasis);
   * }
   * }}}
  */
  def `alert-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-pinterest {
   *   --tblr-alert-color:var(--tblr-pinterest-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-pinterest-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-pinterest-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-pinterest-text-emphasis);
   * }
   * }}}
  */
  def `alert-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-primary {
   *   --tblr-alert-color:var(--tblr-primary-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-primary-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-primary-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-primary-text-emphasis);
   * }
   * }}}
  */
  def `alert-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-purple {
   *   --tblr-alert-color:var(--tblr-purple-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-purple-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-purple-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-purple-text-emphasis);
   * }
   * }}}
  */
  def `alert-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-red {
   *   --tblr-alert-color:var(--tblr-red-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-red-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-red-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-red-text-emphasis);
   * }
   * }}}
  */
  def `alert-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-rss {
   *   --tblr-alert-color:var(--tblr-rss-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-rss-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-rss-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-rss-text-emphasis);
   * }
   * }}}
  */
  def `alert-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-secondary {
   *   --tblr-alert-color:var(--tblr-secondary-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-secondary-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-secondary-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-secondary-text-emphasis);
   * }
   * }}}
  */
  def `alert-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-success {
   *   --tblr-alert-color:var(--tblr-success-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-success-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-success-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-success-text-emphasis);
   * }
   * }}}
  */
  def `alert-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-tabler {
   *   --tblr-alert-color:var(--tblr-tabler-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-tabler-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-tabler-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-tabler-text-emphasis);
   * }
   * }}}
  */
  def `alert-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-teal {
   *   --tblr-alert-color:var(--tblr-teal-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-teal-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-teal-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-teal-text-emphasis);
   * }
   * }}}
  */
  def `alert-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-important .alert-icon,
   * .alert-important .alert-link,
   * .alert-important .alert-title { color:inherit; }
   * }}}
  */
  def `alert-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-twitter {
   *   --tblr-alert-color:var(--tblr-twitter-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-twitter-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-twitter-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-twitter-text-emphasis);
   * }
   * }}}
  */
  def `alert-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-vimeo {
   *   --tblr-alert-color:var(--tblr-vimeo-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-vimeo-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-vimeo-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-vimeo-text-emphasis);
   * }
   * }}}
  */
  def `alert-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-vk {
   *   --tblr-alert-color:var(--tblr-vk-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-vk-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-vk-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-vk-text-emphasis);
   * }
   * }}}
  */
  def `alert-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-warning {
   *   --tblr-alert-color:var(--tblr-warning-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-warning-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-warning-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-warning-text-emphasis);
   * }
   * }}}
  */
  def `alert-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-yellow {
   *   --tblr-alert-color:var(--tblr-yellow-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-yellow-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-yellow-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-yellow-text-emphasis);
   * }
   * }}}
  */
  def `alert-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-youtube {
   *   --tblr-alert-color:var(--tblr-youtube-text-emphasis);
   *   --tblr-alert-bg:var(--tblr-youtube-bg-subtle);
   *   --tblr-alert-border-color:var(--tblr-youtube-border-subtle);
   *   --tblr-alert-link-color:var(--tblr-youtube-text-emphasis);
   * }
   * }}}
  */
  def `alert-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-baseline { vertical-align:baseline !important; }
   * }}}
  */
  def `align-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-bottom { vertical-align:bottom !important; }
   * }}}
  */
  def `align-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-around { align-content:space-around !important; }
   * }}}
  */
  def `align-content-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-between { align-content:space-between !important; }
   * }}}
  */
  def `align-content-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-center { align-content:center !important; }
   * }}}
  */
  def `align-content-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-end { align-content:flex-end !important; }
   * }}}
  */
  def `align-content-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-lg-around { align-content:space-around !important; }
   * }}}
  */
  def `align-content-lg-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-lg-between { align-content:space-between !important; }
   * }}}
  */
  def `align-content-lg-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-lg-center { align-content:center !important; }
   * }}}
  */
  def `align-content-lg-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-lg-end { align-content:flex-end !important; }
   * }}}
  */
  def `align-content-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-lg-start { align-content:flex-start !important; }
   * }}}
  */
  def `align-content-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-lg-stretch { align-content:stretch !important; }
   * }}}
  */
  def `align-content-lg-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-md-around { align-content:space-around !important; }
   * }}}
  */
  def `align-content-md-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-md-between { align-content:space-between !important; }
   * }}}
  */
  def `align-content-md-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-md-center { align-content:center !important; }
   * }}}
  */
  def `align-content-md-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-md-end { align-content:flex-end !important; }
   * }}}
  */
  def `align-content-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-md-start { align-content:flex-start !important; }
   * }}}
  */
  def `align-content-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-md-stretch { align-content:stretch !important; }
   * }}}
  */
  def `align-content-md-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-sm-around { align-content:space-around !important; }
   * }}}
  */
  def `align-content-sm-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-sm-between { align-content:space-between !important; }
   * }}}
  */
  def `align-content-sm-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-sm-center { align-content:center !important; }
   * }}}
  */
  def `align-content-sm-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-sm-end { align-content:flex-end !important; }
   * }}}
  */
  def `align-content-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-sm-start { align-content:flex-start !important; }
   * }}}
  */
  def `align-content-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-sm-stretch { align-content:stretch !important; }
   * }}}
  */
  def `align-content-sm-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-start { align-content:flex-start !important; }
   * }}}
  */
  def `align-content-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-stretch { align-content:stretch !important; }
   * }}}
  */
  def `align-content-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xl-around { align-content:space-around !important; }
   * }}}
  */
  def `align-content-xl-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xl-between { align-content:space-between !important; }
   * }}}
  */
  def `align-content-xl-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xl-center { align-content:center !important; }
   * }}}
  */
  def `align-content-xl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xl-end { align-content:flex-end !important; }
   * }}}
  */
  def `align-content-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xl-start { align-content:flex-start !important; }
   * }}}
  */
  def `align-content-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xl-stretch { align-content:stretch !important; }
   * }}}
  */
  def `align-content-xl-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xxl-around { align-content:space-around !important; }
   * }}}
  */
  def `align-content-xxl-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xxl-between { align-content:space-between !important; }
   * }}}
  */
  def `align-content-xxl-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xxl-center { align-content:center !important; }
   * }}}
  */
  def `align-content-xxl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xxl-end { align-content:flex-end !important; }
   * }}}
  */
  def `align-content-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xxl-start { align-content:flex-start !important; }
   * }}}
  */
  def `align-content-xxl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-content-xxl-stretch { align-content:stretch !important; }
   * }}}
  */
  def `align-content-xxl-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-baseline { align-items:baseline !important; }
   * }}}
  */
  def `align-items-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-center { align-items:center !important; }
   * }}}
  */
  def `align-items-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-end { align-items:flex-end !important; }
   * }}}
  */
  def `align-items-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-lg-baseline { align-items:baseline !important; }
   * }}}
  */
  def `align-items-lg-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-lg-center { align-items:center !important; }
   * }}}
  */
  def `align-items-lg-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-lg-end { align-items:flex-end !important; }
   * }}}
  */
  def `align-items-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-lg-start { align-items:flex-start !important; }
   * }}}
  */
  def `align-items-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-lg-stretch { align-items:stretch !important; }
   * }}}
  */
  def `align-items-lg-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-md-baseline { align-items:baseline !important; }
   * }}}
  */
  def `align-items-md-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-md-center { align-items:center !important; }
   * }}}
  */
  def `align-items-md-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-md-end { align-items:flex-end !important; }
   * }}}
  */
  def `align-items-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-md-start { align-items:flex-start !important; }
   * }}}
  */
  def `align-items-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-md-stretch { align-items:stretch !important; }
   * }}}
  */
  def `align-items-md-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-sm-baseline { align-items:baseline !important; }
   * }}}
  */
  def `align-items-sm-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-sm-center { align-items:center !important; }
   * }}}
  */
  def `align-items-sm-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-sm-end { align-items:flex-end !important; }
   * }}}
  */
  def `align-items-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-sm-start { align-items:flex-start !important; }
   * }}}
  */
  def `align-items-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-sm-stretch { align-items:stretch !important; }
   * }}}
  */
  def `align-items-sm-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-start { align-items:flex-start !important; }
   * }}}
  */
  def `align-items-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-stretch { align-items:stretch !important; }
   * }}}
  */
  def `align-items-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xl-baseline { align-items:baseline !important; }
   * }}}
  */
  def `align-items-xl-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xl-center { align-items:center !important; }
   * }}}
  */
  def `align-items-xl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xl-end { align-items:flex-end !important; }
   * }}}
  */
  def `align-items-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xl-start { align-items:flex-start !important; }
   * }}}
  */
  def `align-items-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xl-stretch { align-items:stretch !important; }
   * }}}
  */
  def `align-items-xl-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xxl-baseline { align-items:baseline !important; }
   * }}}
  */
  def `align-items-xxl-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xxl-center { align-items:center !important; }
   * }}}
  */
  def `align-items-xxl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xxl-end { align-items:flex-end !important; }
   * }}}
  */
  def `align-items-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xxl-start { align-items:flex-start !important; }
   * }}}
  */
  def `align-items-xxl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-items-xxl-stretch { align-items:stretch !important; }
   * }}}
  */
  def `align-items-xxl-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-middle { vertical-align:middle !important; }
   * }}}
  */
  def `align-middle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-auto { align-self:auto !important; }
   * }}}
  */
  def `align-self-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-baseline { align-self:baseline !important; }
   * }}}
  */
  def `align-self-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-center { align-self:center !important; }
   * }}}
  */
  def `align-self-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-end { align-self:flex-end !important; }
   * }}}
  */
  def `align-self-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-lg-auto { align-self:auto !important; }
   * }}}
  */
  def `align-self-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-lg-baseline { align-self:baseline !important; }
   * }}}
  */
  def `align-self-lg-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-lg-center { align-self:center !important; }
   * }}}
  */
  def `align-self-lg-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-lg-end { align-self:flex-end !important; }
   * }}}
  */
  def `align-self-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-lg-start { align-self:flex-start !important; }
   * }}}
  */
  def `align-self-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-lg-stretch { align-self:stretch !important; }
   * }}}
  */
  def `align-self-lg-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-md-auto { align-self:auto !important; }
   * }}}
  */
  def `align-self-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-md-baseline { align-self:baseline !important; }
   * }}}
  */
  def `align-self-md-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-md-center { align-self:center !important; }
   * }}}
  */
  def `align-self-md-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-md-end { align-self:flex-end !important; }
   * }}}
  */
  def `align-self-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-md-start { align-self:flex-start !important; }
   * }}}
  */
  def `align-self-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-md-stretch { align-self:stretch !important; }
   * }}}
  */
  def `align-self-md-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-sm-auto { align-self:auto !important; }
   * }}}
  */
  def `align-self-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-sm-baseline { align-self:baseline !important; }
   * }}}
  */
  def `align-self-sm-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-sm-center { align-self:center !important; }
   * }}}
  */
  def `align-self-sm-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-sm-end { align-self:flex-end !important; }
   * }}}
  */
  def `align-self-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-sm-start { align-self:flex-start !important; }
   * }}}
  */
  def `align-self-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-sm-stretch { align-self:stretch !important; }
   * }}}
  */
  def `align-self-sm-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-start { align-self:flex-start !important; }
   * }}}
  */
  def `align-self-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-stretch { align-self:stretch !important; }
   * }}}
  */
  def `align-self-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xl-auto { align-self:auto !important; }
   * }}}
  */
  def `align-self-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xl-baseline { align-self:baseline !important; }
   * }}}
  */
  def `align-self-xl-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xl-center { align-self:center !important; }
   * }}}
  */
  def `align-self-xl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xl-end { align-self:flex-end !important; }
   * }}}
  */
  def `align-self-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xl-start { align-self:flex-start !important; }
   * }}}
  */
  def `align-self-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xl-stretch { align-self:stretch !important; }
   * }}}
  */
  def `align-self-xl-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xxl-auto { align-self:auto !important; }
   * }}}
  */
  def `align-self-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xxl-baseline { align-self:baseline !important; }
   * }}}
  */
  def `align-self-xxl-baseline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xxl-center { align-self:center !important; }
   * }}}
  */
  def `align-self-xxl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xxl-end { align-self:flex-end !important; }
   * }}}
  */
  def `align-self-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xxl-start { align-self:flex-start !important; }
   * }}}
  */
  def `align-self-xxl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-self-xxl-stretch { align-self:stretch !important; }
   * }}}
  */
  def `align-self-xxl-stretch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-text-bottom { vertical-align:text-bottom !important; }
   * }}}
  */
  def `align-text-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-text-top { vertical-align:text-top !important; }
   * }}}
  */
  def `align-text-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .align-top { vertical-align:top !important; }
   * }}}
  */
  def `align-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .animated-dots {
   *   display:inline-block;
   *   overflow:hidden;
   *   vertical-align:bottom;
   * }
   * }}}
  */
  def `animated-dots`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .antialiased {
   *   -webkit-font-smoothing:antialiased;
   *   -moz-osx-font-smoothing:grayscale;
   * }
   * }}}
  */
  def antialiased: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-gridline { stroke:var(--tblr-border-color) !important; }
   * }}}
  */
  def `apexcharts-gridline`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-legend-text { color:inherit !important; }
   * }}}
  */
  def `apexcharts-legend-text`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-radialbar-area { stroke:var(--tblr-border-color-dark) !important; }
   * }}}
  */
  def `apexcharts-radialbar-area`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-text { fill:var(--tblr-body-color) !important; }
   * }}}
  */
  def `apexcharts-text`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-tooltip {
   *   color:var(--tblr-light) !important;
   *   background:var(--tblr-bg-surface-dark) !important;
   *   font-size:.765625rem !important;
   *   padding:.25rem !important;
   *   box-shadow:none !important;
   * }
   * }}}
  */
  def `apexcharts-tooltip`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-tooltip-marker {
   *   width:10px !important;
   *   height:10px !important;
   * }
   * }}}
  */
  def `apexcharts-tooltip-marker`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-tooltip-series-group { padding:0 .5rem 0 !important; }
   * }}}
  */
  def `apexcharts-tooltip-series-group`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-tooltip-title {
   *   background:0 0 !important;
   *   border:0 !important;
   *   margin:0 !important;
   *   font-weight:var(--tblr-font-weight-bold);
   *   padding:.25rem .5rem !important;
   * }
   * }}}
  */
  def `apexcharts-tooltip-title`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .apexcharts-tooltip-y-group { padding:2px 0 !important; }
   * }}}
  */
  def `apexcharts-tooltip-y-group`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-sort.asc,
   * .table-sort.desc,
   * .table-sort:hover { color:var(--tblr-body-color); }
   * }}}
  */
  def asc: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar {
   *   --tblr-avatar-size:2.5rem;
   *   --tblr-avatar-status-size:0.75rem;
   *   --tblr-avatar-bg:var(--tblr-bg-surface-secondary);
   *   --tblr-avatar-box-shadow:var(--tblr-box-shadow-border);
   *   --tblr-avatar-font-size:1rem;
   *   --tblr-avatar-icon-size:1.5rem;
   *   position:relative;
   *   width:var(--tblr-avatar-size);
   *   height:var(--tblr-avatar-size);
   *   font-size:var(--tblr-avatar-font-size);
   *   font-weight:var(--tblr-font-weight-medium);
   *   line-height:1;
   *   display:inline-flex;
   *   align-items:center;
   *   justify-content:center;
   *   color:var(--tblr-secondary);
   *   text-align:center;
   *   text-transform:uppercase;
   *   vertical-align:bottom;
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   *   background:var(--tblr-avatar-bg) no-repeat center/cover;
   *   border-radius:var(--tblr-border-radius);
   *   box-shadow:var(--tblr-avatar-box-shadow);
   * }
   * }}}
  */
  def avatar: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-2xl {
   *   --tblr-avatar-size:7rem;
   *   --tblr-avatar-status-size:1rem;
   *   --tblr-avatar-font-size:3rem;
   *   --tblr-avatar-icon-size:5rem;
   * }
   * }}}
  */
  def `avatar-2xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-cover {
   *   margin-top:calc(-.5*var(--tblr-avatar-size));
   *   box-shadow:0 0 0 .25rem var(--tblr-card-bg,var(--tblr-body-bg));
   * }
   * }}}
  */
  def `avatar-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-lg {
   *   --tblr-avatar-size:3rem;
   *   --tblr-avatar-status-size:0.75rem;
   *   --tblr-avatar-font-size:1.25rem;
   *   --tblr-avatar-icon-size:2rem;
   * }
   * }}}
  */
  def `avatar-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-list {
   *   --tblr-list-gap:0.5rem;
   *   display:flex;
   *   flex-wrap:wrap;
   *   gap:var(--tblr-list-gap);
   * }
   * }}}
  */
  def `avatar-list`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-list-stacked {
   *   display:block;
   *   --tblr-list-gap:0;
   * }
   * }}}
  */
  def `avatar-list-stacked`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-md {
   *   --tblr-avatar-size:2.5rem;
   *   --tblr-avatar-status-size:0.75rem;
   *   --tblr-avatar-font-size:0.875rem;
   *   --tblr-avatar-icon-size:1.5rem;
   * }
   * }}}
  */
  def `avatar-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-rounded { border-radius:100rem; }
   * }}}
  */
  def `avatar-rounded`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-sm {
   *   --tblr-avatar-size:2rem;
   *   --tblr-avatar-status-size:0.5rem;
   *   --tblr-avatar-font-size:0.75rem;
   *   --tblr-avatar-icon-size:1.25rem;
   * }
   * }}}
  */
  def `avatar-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-upload {
   *   width:4rem;
   *   height:4rem;
   *   border:var(--tblr-border-width) dashed var(--tblr-border-color);
   *   background:var(--tblr-bg-forms);
   *   flex-direction:column;
   *   transition:color .3s,background-color .3s;
   * }
   * }}}
  */
  def `avatar-upload`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-upload-text {
   *   font-size:.625rem;
   *   line-height:1;
   *   margin-top:.25rem;
   * }
   * }}}
  */
  def `avatar-upload-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-xl {
   *   --tblr-avatar-size:5rem;
   *   --tblr-avatar-status-size:1rem;
   *   --tblr-avatar-font-size:2rem;
   *   --tblr-avatar-icon-size:3rem;
   * }
   * }}}
  */
  def `avatar-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-xs {
   *   --tblr-avatar-size:1.25rem;
   *   --tblr-avatar-status-size:0.375rem;
   *   --tblr-avatar-font-size:0.625rem;
   *   --tblr-avatar-icon-size:1rem;
   * }
   * }}}
  */
  def `avatar-xs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .avatar-xxs {
   *   --tblr-avatar-size:1rem;
   *   --tblr-avatar-status-size:0.25rem;
   *   --tblr-avatar-font-size:0.5rem;
   *   --tblr-avatar-icon-size:0.75rem;
   * }
   * }}}
  */
  def `avatar-xxs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badge {
   *   --tblr-badge-padding-x:0.5em;
   *   --tblr-badge-padding-y:0.25em;
   *   --tblr-badge-font-size:85.714285%;
   *   --tblr-badge-font-weight:var(--tblr-font-weight-medium);
   *   --tblr-badge-color:var(--tblr-secondary);
   *   --tblr-badge-border-radius:var(--tblr-border-radius);
   *   display:inline-block;
   *   padding:var(--tblr-badge-padding-y) var(--tblr-badge-padding-x);
   *   font-size:var(--tblr-badge-font-size);
   *   font-weight:var(--tblr-badge-font-weight);
   *   line-height:1;
   *   color:var(--tblr-badge-color);
   *   text-align:center;
   *   white-space:nowrap;
   *   vertical-align:baseline;
   *   border-radius:var(--tblr-badge-border-radius);
   * }
   * }}}
  */
  def badge: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badge-blink { animation:blink 2s infinite; }
   * }}}
  */
  def `badge-blink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badge-empty,
   * .badge:empty {
   *   display:inline-block;
   *   width:.5rem;
   *   height:.5rem;
   *   min-width:0;
   *   min-height:auto;
   *   padding:0;
   *   border-radius:100rem;
   *   vertical-align:baseline;
   * }
   * }}}
  */
  def `badge-empty`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badge-notification {
   *   position:absolute !important;
   *   top:0 !important;
   *   right:0 !important;
   *   transform:translate(50%,-50%);
   *   z-index:1;
   * }
   * }}}
  */
  def `badge-notification`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badge-outline {
   *   background-color:transparent;
   *   border:var(--tblr-border-width) var(--tblr-border-style) currentColor;
   * }
   * }}}
  */
  def `badge-outline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badge-pill { border-radius:100rem; }
   * }}}
  */
  def `badge-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .badges-list {
   *   --tblr-list-gap:0.5rem;
   *   display:flex;
   *   flex-wrap:wrap;
   *   gap:var(--tblr-list-gap);
   * }
   * }}}
  */
  def `badges-list`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-azure {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-azure-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-azure-lt { border-color:rgba(var(--tblr-azure-rgb),.1) !important; }
   * }}}
  */
  def `bg-azure-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-bitbucket {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-bitbucket-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-bitbucket-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-bitbucket-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-black {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-black-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-black`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-blue {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-blue-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-blue-lt { border-color:rgba(var(--tblr-blue-rgb),.1) !important; }
   * }}}
  */
  def `bg-blue-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-body {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-body-bg-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-body-secondary {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-secondary-bg-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-body-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-body-tertiary {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-tertiary-bg-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-body-tertiary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-cover {
   *   background-repeat:no-repeat;
   *   background-size:cover;
   *   background-position:center;
   * }
   * }}}
  */
  def `bg-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-cyan {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-cyan-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-cyan-lt { border-color:rgba(var(--tblr-cyan-rgb),.1) !important; }
   * }}}
  */
  def `bg-cyan-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-danger {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-danger-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-danger-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-danger-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-danger-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-danger-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-danger-subtle { background-color:var(--tblr-danger-bg-subtle) !important; }
   * }}}
  */
  def `bg-danger-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-dark {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-dark-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-dark-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-dark-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-dark-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-dark-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-dark-overlay {
   *   color:#fff;
   *   background-color:rgba(24,36,51,.24);
   * }
   * }}}
  */
  def `bg-dark-overlay`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-dark-subtle { background-color:var(--tblr-dark-bg-subtle) !important; }
   * }}}
  */
  def `bg-dark-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-dribbble {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-dribbble-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-dribbble-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-dribbble-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-dribbble-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-dribbble-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-facebook {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-facebook-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-facebook-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-facebook-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-facebook-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-facebook-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-flickr {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-flickr-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-flickr-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-flickr-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-flickr-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-flickr-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-github {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-github-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-github-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-github-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-github-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-github-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-google {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-google-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-google-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-google-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-google-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-google-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gradient { background-image:var(--tblr-gradient) !important; }
   * }}}
  */
  def `bg-gradient`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-100 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-100-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-200 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-200-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-200`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-300 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-300-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-300`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-400 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-400-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-400`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-50 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-50-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-500 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-500-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-500`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-600 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-600-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-600`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-700 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-700-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-700`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-800 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-800-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-800`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-gray-900 {
   *   --tblr-bg-opacity:.1;
   *   background-color:rgba(var(--tblr-gray-900-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-gray-900`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-green {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-green-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-green-lt { border-color:rgba(var(--tblr-green-rgb),.1) !important; }
   * }}}
  */
  def `bg-green-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-indigo {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-indigo-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-indigo-lt { border-color:rgba(var(--tblr-indigo-rgb),.1) !important; }
   * }}}
  */
  def `bg-indigo-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-info {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-info-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-info-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-info-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-info-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-info-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-info-subtle { background-color:var(--tblr-info-bg-subtle) !important; }
   * }}}
  */
  def `bg-info-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-instagram {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-instagram-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-instagram-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-instagram-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-instagram-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-instagram-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-light {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-light-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-light-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-light-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-light-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-light-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-light-subtle { background-color:var(--tblr-light-bg-subtle) !important; }
   * }}}
  */
  def `bg-light-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-lime {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-lime-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-lime-lt { border-color:rgba(var(--tblr-lime-rgb),.1) !important; }
   * }}}
  */
  def `bg-lime-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-linkedin {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-linkedin-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-linkedin-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-linkedin-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-linkedin-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-linkedin-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-muted {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-muted-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-muted-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-muted-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-muted-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-muted-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-opacity-10 { --tblr-bg-opacity:0.1; }
   * }}}
  */
  def `bg-opacity-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-opacity-100 { --tblr-bg-opacity:1; }
   * }}}
  */
  def `bg-opacity-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-opacity-25 { --tblr-bg-opacity:0.25; }
   * }}}
  */
  def `bg-opacity-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-opacity-50 { --tblr-bg-opacity:0.5; }
   * }}}
  */
  def `bg-opacity-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-opacity-75 { --tblr-bg-opacity:0.75; }
   * }}}
  */
  def `bg-opacity-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-orange {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-orange-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-orange-lt { border-color:rgba(var(--tblr-orange-rgb),.1) !important; }
   * }}}
  */
  def `bg-orange-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-pink {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-pink-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-pink-lt { border-color:rgba(var(--tblr-pink-rgb),.1) !important; }
   * }}}
  */
  def `bg-pink-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-pinterest {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-pinterest-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-pinterest-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-pinterest-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-pinterest-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-pinterest-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-primary {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-primary-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-primary-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-primary-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-primary-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-primary-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-primary-subtle { background-color:var(--tblr-primary-bg-subtle) !important; }
   * }}}
  */
  def `bg-primary-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-purple {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-purple-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-purple-lt { border-color:rgba(var(--tblr-purple-rgb),.1) !important; }
   * }}}
  */
  def `bg-purple-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-red {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-red-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-red-lt { border-color:rgba(var(--tblr-red-rgb),.1) !important; }
   * }}}
  */
  def `bg-red-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-rss {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-rss-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-rss-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-rss-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-rss-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-rss-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-secondary {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-secondary-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-secondary-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-secondary-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-secondary-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-secondary-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-secondary-subtle { background-color:var(--tblr-secondary-bg-subtle) !important; }
   * }}}
  */
  def `bg-secondary-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-success {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-success-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-success-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-success-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-success-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-success-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-success-subtle { background-color:var(--tblr-success-bg-subtle) !important; }
   * }}}
  */
  def `bg-success-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-tabler {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-tabler-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-tabler-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-tabler-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-tabler-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-tabler-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-teal {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-teal-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-teal-lt { border-color:rgba(var(--tblr-teal-rgb),.1) !important; }
   * }}}
  */
  def `bg-teal-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-transparent {
   *   --tblr-bg-opacity:1;
   *   background-color:transparent !important;
   * }
   * }}}
  */
  def `bg-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-twitter {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-twitter-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-twitter-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-twitter-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-twitter-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-twitter-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-vimeo {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-vimeo-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-vimeo-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-vimeo-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-vimeo-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-vimeo-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-vk {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-vk-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-vk-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-vk-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-vk-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-vk-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-warning {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-warning-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-warning-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-warning-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-warning-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-warning-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-warning-subtle { background-color:var(--tblr-warning-bg-subtle) !important; }
   * }}}
  */
  def `bg-warning-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-white {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-white-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-white`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-white-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-white-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-white-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-white-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-white-overlay {
   *   color:#fff;
   *   background-color:rgba(252,253,254,.24);
   * }
   * }}}
  */
  def `bg-white-overlay`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-yellow {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-yellow-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon.bg-yellow-lt { border-color:rgba(var(--tblr-yellow-rgb),.1) !important; }
   * }}}
  */
  def `bg-yellow-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-youtube {
   *   --tblr-bg-opacity:1;
   *   background-color:rgba(var(--tblr-youtube-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bg-youtube-lt {
   *   --tblr-bg-opacity:1;
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-youtube-rgb),var(--tblr-text-opacity)) !important;
   *   background-color:rgba(var(--tblr-youtube-lt-rgb),var(--tblr-bg-opacity)) !important;
   * }
   * }}}
  */
  def `bg-youtube-lt`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-link>.bi {
   *   flex-shrink:0;
   *   width:1em;
   *   height:1em;
   *   fill:currentcolor;
   *   transition:.2s ease-in-out transform;
   * }
   * }}}
  */
  def bi: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .blockquote {
   *   margin-bottom:1rem;
   *   font-size:.875rem;
   * }
   * }}}
  */
  def blockquote: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .blockquote-footer {
   *   margin-top:-1rem;
   *   margin-bottom:1rem;
   *   font-size:85.714285%;
   *   color:#667382;
   * }
   * }}}
  */
  def `blockquote-footer`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border { border:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def border: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-0 { border:0 !important; }
   * }}}
  */
  def `border-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-1 { border-width:1px !important; }
   * }}}
  */
  def `border-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-2 { border-width:2px !important; }
   * }}}
  */
  def `border-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-3 { border-width:3px !important; }
   * }}}
  */
  def `border-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-4 { border-width:4px !important; }
   * }}}
  */
  def `border-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-5 { border-width:5px !important; }
   * }}}
  */
  def `border-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-azure {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-azure-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-bitbucket {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-black {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-black-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-black`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-blue {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-blue-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-bottom { border-bottom:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-bottom-0 { border-bottom:0 !important; }
   * }}}
  */
  def `border-bottom-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-bottom-wide { border-bottom:2px var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-bottom-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-cyan {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-cyan-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-danger {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-danger-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-danger-subtle { border-color:var(--tblr-danger-border-subtle) !important; }
   * }}}
  */
  def `border-danger-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-dark {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-dark-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-dark-subtle { border-color:var(--tblr-dark-border-subtle) !important; }
   * }}}
  */
  def `border-dark-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-dribbble {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-dribbble-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-end { border-right:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-end-0 { border-right:0 !important; }
   * }}}
  */
  def `border-end-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-end-wide { border-right:2px var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-end-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-facebook {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-facebook-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-flickr {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-flickr-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-github {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-github-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-google {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-google-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-green {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-green-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-indigo {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-indigo-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-info {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-info-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-info-subtle { border-color:var(--tblr-info-border-subtle) !important; }
   * }}}
  */
  def `border-info-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-instagram {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-instagram-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-light {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-light-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-light-subtle { border-color:var(--tblr-light-border-subtle) !important; }
   * }}}
  */
  def `border-light-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-lime {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-lime-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-linkedin {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-linkedin-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-muted {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-muted-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-opacity-10 { --tblr-border-opacity:0.1; }
   * }}}
  */
  def `border-opacity-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-opacity-100 { --tblr-border-opacity:1; }
   * }}}
  */
  def `border-opacity-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-opacity-25 { --tblr-border-opacity:0.25; }
   * }}}
  */
  def `border-opacity-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-opacity-50 { --tblr-border-opacity:0.5; }
   * }}}
  */
  def `border-opacity-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-opacity-75 { --tblr-border-opacity:0.75; }
   * }}}
  */
  def `border-opacity-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-orange {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-orange-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-pink {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-pink-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-pinterest {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-pinterest-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-primary {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-primary-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-primary-subtle { border-color:var(--tblr-primary-border-subtle) !important; }
   * }}}
  */
  def `border-primary-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-purple {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-purple-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-red {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-red-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-rss {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-rss-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-secondary {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-secondary-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-secondary-subtle { border-color:var(--tblr-secondary-border-subtle) !important; }
   * }}}
  */
  def `border-secondary-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-start { border-left:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-start-0 { border-left:0 !important; }
   * }}}
  */
  def `border-start-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-start-wide { border-left:2px var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-start-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-success {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-success-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-success-subtle { border-color:var(--tblr-success-border-subtle) !important; }
   * }}}
  */
  def `border-success-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-tabler {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-tabler-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-teal {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-teal-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-top { border-top:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-top-0 { border-top:0 !important; }
   * }}}
  */
  def `border-top-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-top-wide { border-top:2px var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-top-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-twitter {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-twitter-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-vimeo {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-vimeo-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-vk {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-vk-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-warning {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-warning-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-warning-subtle { border-color:var(--tblr-warning-border-subtle) !important; }
   * }}}
  */
  def `border-warning-subtle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-white {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-white-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-white`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-wide { border:2px var(--tblr-border-style) rgba(4,32,69,.14) !important; }
   * }}}
  */
  def `border-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-x {
   *   border-left:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important;
   *   border-right:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important;
   * }
   * }}}
  */
  def `border-x`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-x-0 {
   *   border-left:0 !important;
   *   border-right:0 !important;
   * }
   * }}}
  */
  def `border-x-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-x-wide {
   *   border-left:2px var(--tblr-border-style) rgba(4,32,69,.14) !important;
   *   border-right:2px var(--tblr-border-style) rgba(4,32,69,.14) !important;
   * }
   * }}}
  */
  def `border-x-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-y {
   *   border-top:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important;
   *   border-bottom:var(--tblr-border-width) var(--tblr-border-style) rgba(4,32,69,.14) !important;
   * }
   * }}}
  */
  def `border-y`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-y-0 {
   *   border-top:0 !important;
   *   border-bottom:0 !important;
   * }
   * }}}
  */
  def `border-y-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-y-wide {
   *   border-top:2px var(--tblr-border-style) rgba(4,32,69,.14) !important;
   *   border-bottom:2px var(--tblr-border-style) rgba(4,32,69,.14) !important;
   * }
   * }}}
  */
  def `border-y-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-yellow {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-yellow-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .border-youtube {
   *   --tblr-border-opacity:1;
   *   border-color:rgba(var(--tblr-youtube-rgb),var(--tblr-border-opacity)) !important;
   * }
   * }}}
  */
  def `border-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bottom-0 { bottom:0 !important; }
   * }}}
  */
  def `bottom-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bottom-100 { bottom:100% !important; }
   * }}}
  */
  def `bottom-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bottom-50 { bottom:50% !important; }
   * }}}
  */
  def `bottom-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .breadcrumb {
   *   --tblr-breadcrumb-padding-x:0;
   *   --tblr-breadcrumb-padding-y:0;
   *   --tblr-breadcrumb-margin-bottom:1rem;
   *   --tblr-breadcrumb-divider-color:var(--tblr-secondary);
   *   --tblr-breadcrumb-item-padding-x:0.5rem;
   *   --tblr-breadcrumb-item-active-color:inherit;
   *   display:flex;
   *   flex-wrap:wrap;
   *   padding:var(--tblr-breadcrumb-padding-y) var(--tblr-breadcrumb-padding-x);
   *   margin-bottom:var(--tblr-breadcrumb-margin-bottom);
   *   font-size:var(--tblr-breadcrumb-font-size);
   *   list-style:none;
   *   background-color:var(--tblr-breadcrumb-bg);
   *   border-radius:var(--tblr-breadcrumb-border-radius);
   * }
   * }}}
  */
  def breadcrumb: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .breadcrumb-arrows { --tblr-breadcrumb-divider:"›"; }
   * }}}
  */
  def `breadcrumb-arrows`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .breadcrumb-bullets { --tblr-breadcrumb-divider:"•"; }
   * }}}
  */
  def `breadcrumb-bullets`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .breadcrumb-dots { --tblr-breadcrumb-divider:"·"; }
   * }}}
  */
  def `breadcrumb-dots`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .breadcrumb-item+.breadcrumb-item { padding-left:var(--tblr-breadcrumb-item-padding-x); }
   * }}}
  */
  def `breadcrumb-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .breadcrumb-muted { --tblr-breadcrumb-link-color:var(--tblr-secondary); }
   * }}}
  */
  def `breadcrumb-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-popover-auto[data-popper-placement^=top]>.popover-arrow,
   * .bs-popover-top>.popover-arrow { bottom:calc(-1*(var(--tblr-popover-arrow-height)) - var(--tblr-popover-border-width)); }
   * }}}
  */
  def `bs-popover-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-popover-auto[data-popper-placement^=bottom]>.popover-arrow,
   * .bs-popover-bottom>.popover-arrow { top:calc(-1*(var(--tblr-popover-arrow-height)) - var(--tblr-popover-border-width)); }
   * }}}
  */
  def `bs-popover-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-popover-auto[data-popper-placement^=right]>.popover-arrow,
   * .bs-popover-end>.popover-arrow {
   *   left:calc(-1*(var(--tblr-popover-arrow-height)) - var(--tblr-popover-border-width));
   *   width:var(--tblr-popover-arrow-height);
   *   height:var(--tblr-popover-arrow-width);
   * }
   * }}}
  */
  def `bs-popover-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-popover-auto[data-popper-placement^=left]>.popover-arrow,
   * .bs-popover-start>.popover-arrow {
   *   right:calc(-1*(var(--tblr-popover-arrow-height)) - var(--tblr-popover-border-width));
   *   width:var(--tblr-popover-arrow-height);
   *   height:var(--tblr-popover-arrow-width);
   * }
   * }}}
  */
  def `bs-popover-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-popover-auto[data-popper-placement^=top]>.popover-arrow,
   * .bs-popover-top>.popover-arrow { bottom:calc(-1*(var(--tblr-popover-arrow-height)) - var(--tblr-popover-border-width)); }
   * }}}
  */
  def `bs-popover-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-tooltip-auto[data-popper-placement^=top] .tooltip-arrow,
   * .bs-tooltip-top .tooltip-arrow { bottom:calc(-1*var(--tblr-tooltip-arrow-height)); }
   * }}}
  */
  def `bs-tooltip-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-tooltip-auto[data-popper-placement^=bottom] .tooltip-arrow,
   * .bs-tooltip-bottom .tooltip-arrow { top:calc(-1*var(--tblr-tooltip-arrow-height)); }
   * }}}
  */
  def `bs-tooltip-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-tooltip-auto[data-popper-placement^=right] .tooltip-arrow,
   * .bs-tooltip-end .tooltip-arrow {
   *   left:calc(-1*var(--tblr-tooltip-arrow-height));
   *   width:var(--tblr-tooltip-arrow-height);
   *   height:var(--tblr-tooltip-arrow-width);
   * }
   * }}}
  */
  def `bs-tooltip-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-tooltip-auto[data-popper-placement^=left] .tooltip-arrow,
   * .bs-tooltip-start .tooltip-arrow {
   *   right:calc(-1*var(--tblr-tooltip-arrow-height));
   *   width:var(--tblr-tooltip-arrow-height);
   *   height:var(--tblr-tooltip-arrow-width);
   * }
   * }}}
  */
  def `bs-tooltip-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-tooltip-auto[data-popper-placement^=top] .tooltip-arrow,
   * .bs-tooltip-top .tooltip-arrow { bottom:calc(-1*var(--tblr-tooltip-arrow-height)); }
   * }}}
  */
  def `bs-tooltip-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-check:disabled+.btn,
   * .btn-check[disabled]+.btn {
   *   pointer-events:none;
   *   filter:none;
   *   opacity:.4;
   * }
   * }}}
  */
  def btn: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-action {
   *   padding:0;
   *   border:0;
   *   color:var(--tblr-secondary);
   *   display:inline-flex;
   *   width:2rem;
   *   height:2rem;
   *   align-items:center;
   *   justify-content:center;
   *   border-radius:var(--tblr-border-radius);
   *   background:0 0;
   * }
   * }}}
  */
  def `btn-action`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-actions { display:flex; }
   * }}}
  */
  def `btn-actions`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-azure {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-azure-fg);
   *   --tblr-btn-bg:var(--tblr-azure);
   *   --tblr-btn-hover-color:var(--tblr-azure-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-azure-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-azure-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-azure-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-azure);
   *   --tblr-btn-disabled-color:var(--tblr-azure-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-bitbucket {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-bg:var(--tblr-bitbucket);
   *   --tblr-btn-hover-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-bitbucket-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-bitbucket-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-bitbucket);
   *   --tblr-btn-disabled-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-blue {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-blue-fg);
   *   --tblr-btn-bg:var(--tblr-blue);
   *   --tblr-btn-hover-color:var(--tblr-blue-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-blue-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-blue-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-blue-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-blue);
   *   --tblr-btn-disabled-color:var(--tblr-blue-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-check {
   *   position:absolute;
   *   clip:rect(0,0,0,0);
   *   pointer-events:none;
   * }
   * }}}
  */
  def `btn-check`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .alert-dismissible .btn-close {
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   z-index:2;
   *   padding:.9375rem 1rem;
   * }
   * }}}
  */
  def `btn-close`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-close-white { filter:var(--tblr-btn-close-white-filter); }
   * }}}
  */
  def `btn-close-white`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-cyan {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-cyan-fg);
   *   --tblr-btn-bg:var(--tblr-cyan);
   *   --tblr-btn-hover-color:var(--tblr-cyan-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-cyan-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-cyan-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-cyan-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-cyan);
   *   --tblr-btn-disabled-color:var(--tblr-cyan-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-danger {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-danger-fg);
   *   --tblr-btn-bg:var(--tblr-danger);
   *   --tblr-btn-hover-color:var(--tblr-danger-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-danger-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-danger-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-danger-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-danger);
   *   --tblr-btn-disabled-color:var(--tblr-danger-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-dark {
   *   --tblr-btn-border-color:var(--tblr-dark-mode-border-color);
   *   --tblr-btn-hover-border-color:var(--tblr-dark-mode-border-color-active);
   *   --tblr-btn-active-border-color:var(--tblr-dark-mode-border-color-active);
   *   --tblr-btn-color:var(--tblr-dark-fg);
   *   --tblr-btn-bg:var(--tblr-dark);
   *   --tblr-btn-hover-color:var(--tblr-dark-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-dark-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-dark-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-dark-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-dark);
   *   --tblr-btn-disabled-color:var(--tblr-dark-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-dribbble {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-bg:var(--tblr-dribbble);
   *   --tblr-btn-hover-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-dribbble-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-dribbble-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-dribbble);
   *   --tblr-btn-disabled-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-facebook {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-facebook-fg);
   *   --tblr-btn-bg:var(--tblr-facebook);
   *   --tblr-btn-hover-color:var(--tblr-facebook-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-facebook-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-facebook-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-facebook-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-facebook);
   *   --tblr-btn-disabled-color:var(--tblr-facebook-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-flickr {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-flickr-fg);
   *   --tblr-btn-bg:var(--tblr-flickr);
   *   --tblr-btn-hover-color:var(--tblr-flickr-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-flickr-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-flickr-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-flickr-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-flickr);
   *   --tblr-btn-disabled-color:var(--tblr-flickr-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-floating {
   *   position:fixed;
   *   z-index:1030;
   *   bottom:1.5rem;
   *   right:1.5rem;
   *   border-radius:100rem;
   * }
   * }}}
  */
  def `btn-floating`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-azure {
   *   --tblr-btn-color:var(--tblr-azure);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-azure-fg);
   *   --tblr-btn-hover-bg:var(--tblr-azure);
   *   --tblr-btn-hover-border-color:var(--tblr-azure);
   *   --tblr-btn-active-color:var(--tblr-azure-fg);
   *   --tblr-btn-active-bg:var(--tblr-azure);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-azure);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-bitbucket {
   *   --tblr-btn-color:var(--tblr-bitbucket);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-hover-bg:var(--tblr-bitbucket);
   *   --tblr-btn-hover-border-color:var(--tblr-bitbucket);
   *   --tblr-btn-active-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-active-bg:var(--tblr-bitbucket);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-bitbucket);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-blue {
   *   --tblr-btn-color:var(--tblr-blue);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-blue-fg);
   *   --tblr-btn-hover-bg:var(--tblr-blue);
   *   --tblr-btn-hover-border-color:var(--tblr-blue);
   *   --tblr-btn-active-color:var(--tblr-blue-fg);
   *   --tblr-btn-active-bg:var(--tblr-blue);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-blue);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-cyan {
   *   --tblr-btn-color:var(--tblr-cyan);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-cyan-fg);
   *   --tblr-btn-hover-bg:var(--tblr-cyan);
   *   --tblr-btn-hover-border-color:var(--tblr-cyan);
   *   --tblr-btn-active-color:var(--tblr-cyan-fg);
   *   --tblr-btn-active-bg:var(--tblr-cyan);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-cyan);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-danger {
   *   --tblr-btn-color:var(--tblr-danger);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-danger-fg);
   *   --tblr-btn-hover-bg:var(--tblr-danger);
   *   --tblr-btn-hover-border-color:var(--tblr-danger);
   *   --tblr-btn-active-color:var(--tblr-danger-fg);
   *   --tblr-btn-active-bg:var(--tblr-danger);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-danger);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-dark {
   *   --tblr-btn-color:var(--tblr-dark);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-dark-fg);
   *   --tblr-btn-hover-bg:var(--tblr-dark);
   *   --tblr-btn-hover-border-color:var(--tblr-dark);
   *   --tblr-btn-active-color:var(--tblr-dark-fg);
   *   --tblr-btn-active-bg:var(--tblr-dark);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-dark);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-dribbble {
   *   --tblr-btn-color:var(--tblr-dribbble);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-hover-bg:var(--tblr-dribbble);
   *   --tblr-btn-hover-border-color:var(--tblr-dribbble);
   *   --tblr-btn-active-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-active-bg:var(--tblr-dribbble);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-dribbble);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-facebook {
   *   --tblr-btn-color:var(--tblr-facebook);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-facebook-fg);
   *   --tblr-btn-hover-bg:var(--tblr-facebook);
   *   --tblr-btn-hover-border-color:var(--tblr-facebook);
   *   --tblr-btn-active-color:var(--tblr-facebook-fg);
   *   --tblr-btn-active-bg:var(--tblr-facebook);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-facebook);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-flickr {
   *   --tblr-btn-color:var(--tblr-flickr);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-flickr-fg);
   *   --tblr-btn-hover-bg:var(--tblr-flickr);
   *   --tblr-btn-hover-border-color:var(--tblr-flickr);
   *   --tblr-btn-active-color:var(--tblr-flickr-fg);
   *   --tblr-btn-active-bg:var(--tblr-flickr);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-flickr);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-github {
   *   --tblr-btn-color:var(--tblr-github);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-github-fg);
   *   --tblr-btn-hover-bg:var(--tblr-github);
   *   --tblr-btn-hover-border-color:var(--tblr-github);
   *   --tblr-btn-active-color:var(--tblr-github-fg);
   *   --tblr-btn-active-bg:var(--tblr-github);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-github);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-google {
   *   --tblr-btn-color:var(--tblr-google);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-google-fg);
   *   --tblr-btn-hover-bg:var(--tblr-google);
   *   --tblr-btn-hover-border-color:var(--tblr-google);
   *   --tblr-btn-active-color:var(--tblr-google-fg);
   *   --tblr-btn-active-bg:var(--tblr-google);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-google);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-green {
   *   --tblr-btn-color:var(--tblr-green);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-green-fg);
   *   --tblr-btn-hover-bg:var(--tblr-green);
   *   --tblr-btn-hover-border-color:var(--tblr-green);
   *   --tblr-btn-active-color:var(--tblr-green-fg);
   *   --tblr-btn-active-bg:var(--tblr-green);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-green);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-indigo {
   *   --tblr-btn-color:var(--tblr-indigo);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-indigo-fg);
   *   --tblr-btn-hover-bg:var(--tblr-indigo);
   *   --tblr-btn-hover-border-color:var(--tblr-indigo);
   *   --tblr-btn-active-color:var(--tblr-indigo-fg);
   *   --tblr-btn-active-bg:var(--tblr-indigo);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-indigo);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-info {
   *   --tblr-btn-color:var(--tblr-info);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-info-fg);
   *   --tblr-btn-hover-bg:var(--tblr-info);
   *   --tblr-btn-hover-border-color:var(--tblr-info);
   *   --tblr-btn-active-color:var(--tblr-info-fg);
   *   --tblr-btn-active-bg:var(--tblr-info);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-info);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-instagram {
   *   --tblr-btn-color:var(--tblr-instagram);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-instagram-fg);
   *   --tblr-btn-hover-bg:var(--tblr-instagram);
   *   --tblr-btn-hover-border-color:var(--tblr-instagram);
   *   --tblr-btn-active-color:var(--tblr-instagram-fg);
   *   --tblr-btn-active-bg:var(--tblr-instagram);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-instagram);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-light {
   *   --tblr-btn-color:var(--tblr-light);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-light-fg);
   *   --tblr-btn-hover-bg:var(--tblr-light);
   *   --tblr-btn-hover-border-color:var(--tblr-light);
   *   --tblr-btn-active-color:var(--tblr-light-fg);
   *   --tblr-btn-active-bg:var(--tblr-light);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-light);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-lime {
   *   --tblr-btn-color:var(--tblr-lime);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-lime-fg);
   *   --tblr-btn-hover-bg:var(--tblr-lime);
   *   --tblr-btn-hover-border-color:var(--tblr-lime);
   *   --tblr-btn-active-color:var(--tblr-lime-fg);
   *   --tblr-btn-active-bg:var(--tblr-lime);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-lime);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-linkedin {
   *   --tblr-btn-color:var(--tblr-linkedin);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-hover-bg:var(--tblr-linkedin);
   *   --tblr-btn-hover-border-color:var(--tblr-linkedin);
   *   --tblr-btn-active-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-active-bg:var(--tblr-linkedin);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-linkedin);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-muted {
   *   --tblr-btn-color:var(--tblr-muted);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-muted-fg);
   *   --tblr-btn-hover-bg:var(--tblr-muted);
   *   --tblr-btn-hover-border-color:var(--tblr-muted);
   *   --tblr-btn-active-color:var(--tblr-muted-fg);
   *   --tblr-btn-active-bg:var(--tblr-muted);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-muted);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-orange {
   *   --tblr-btn-color:var(--tblr-orange);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-orange-fg);
   *   --tblr-btn-hover-bg:var(--tblr-orange);
   *   --tblr-btn-hover-border-color:var(--tblr-orange);
   *   --tblr-btn-active-color:var(--tblr-orange-fg);
   *   --tblr-btn-active-bg:var(--tblr-orange);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-orange);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-pink {
   *   --tblr-btn-color:var(--tblr-pink);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-pink-fg);
   *   --tblr-btn-hover-bg:var(--tblr-pink);
   *   --tblr-btn-hover-border-color:var(--tblr-pink);
   *   --tblr-btn-active-color:var(--tblr-pink-fg);
   *   --tblr-btn-active-bg:var(--tblr-pink);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-pink);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-pinterest {
   *   --tblr-btn-color:var(--tblr-pinterest);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-hover-bg:var(--tblr-pinterest);
   *   --tblr-btn-hover-border-color:var(--tblr-pinterest);
   *   --tblr-btn-active-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-active-bg:var(--tblr-pinterest);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-pinterest);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-primary {
   *   --tblr-btn-color:var(--tblr-primary);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-primary-fg);
   *   --tblr-btn-hover-bg:var(--tblr-primary);
   *   --tblr-btn-hover-border-color:var(--tblr-primary);
   *   --tblr-btn-active-color:var(--tblr-primary-fg);
   *   --tblr-btn-active-bg:var(--tblr-primary);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-primary);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-purple {
   *   --tblr-btn-color:var(--tblr-purple);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-purple-fg);
   *   --tblr-btn-hover-bg:var(--tblr-purple);
   *   --tblr-btn-hover-border-color:var(--tblr-purple);
   *   --tblr-btn-active-color:var(--tblr-purple-fg);
   *   --tblr-btn-active-bg:var(--tblr-purple);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-purple);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-red {
   *   --tblr-btn-color:var(--tblr-red);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-red-fg);
   *   --tblr-btn-hover-bg:var(--tblr-red);
   *   --tblr-btn-hover-border-color:var(--tblr-red);
   *   --tblr-btn-active-color:var(--tblr-red-fg);
   *   --tblr-btn-active-bg:var(--tblr-red);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-red);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-rss {
   *   --tblr-btn-color:var(--tblr-rss);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-rss-fg);
   *   --tblr-btn-hover-bg:var(--tblr-rss);
   *   --tblr-btn-hover-border-color:var(--tblr-rss);
   *   --tblr-btn-active-color:var(--tblr-rss-fg);
   *   --tblr-btn-active-bg:var(--tblr-rss);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-rss);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-secondary {
   *   --tblr-btn-color:var(--tblr-secondary);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-secondary-fg);
   *   --tblr-btn-hover-bg:var(--tblr-secondary);
   *   --tblr-btn-hover-border-color:var(--tblr-secondary);
   *   --tblr-btn-active-color:var(--tblr-secondary-fg);
   *   --tblr-btn-active-bg:var(--tblr-secondary);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-secondary);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-success {
   *   --tblr-btn-color:var(--tblr-success);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-success-fg);
   *   --tblr-btn-hover-bg:var(--tblr-success);
   *   --tblr-btn-hover-border-color:var(--tblr-success);
   *   --tblr-btn-active-color:var(--tblr-success-fg);
   *   --tblr-btn-active-bg:var(--tblr-success);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-success);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-tabler {
   *   --tblr-btn-color:var(--tblr-tabler);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-tabler-fg);
   *   --tblr-btn-hover-bg:var(--tblr-tabler);
   *   --tblr-btn-hover-border-color:var(--tblr-tabler);
   *   --tblr-btn-active-color:var(--tblr-tabler-fg);
   *   --tblr-btn-active-bg:var(--tblr-tabler);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-tabler);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-teal {
   *   --tblr-btn-color:var(--tblr-teal);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-teal-fg);
   *   --tblr-btn-hover-bg:var(--tblr-teal);
   *   --tblr-btn-hover-border-color:var(--tblr-teal);
   *   --tblr-btn-active-color:var(--tblr-teal-fg);
   *   --tblr-btn-active-bg:var(--tblr-teal);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-teal);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-twitter {
   *   --tblr-btn-color:var(--tblr-twitter);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-twitter-fg);
   *   --tblr-btn-hover-bg:var(--tblr-twitter);
   *   --tblr-btn-hover-border-color:var(--tblr-twitter);
   *   --tblr-btn-active-color:var(--tblr-twitter-fg);
   *   --tblr-btn-active-bg:var(--tblr-twitter);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-twitter);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-vimeo {
   *   --tblr-btn-color:var(--tblr-vimeo);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-hover-bg:var(--tblr-vimeo);
   *   --tblr-btn-hover-border-color:var(--tblr-vimeo);
   *   --tblr-btn-active-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-active-bg:var(--tblr-vimeo);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-vimeo);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-vk {
   *   --tblr-btn-color:var(--tblr-vk);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-vk-fg);
   *   --tblr-btn-hover-bg:var(--tblr-vk);
   *   --tblr-btn-hover-border-color:var(--tblr-vk);
   *   --tblr-btn-active-color:var(--tblr-vk-fg);
   *   --tblr-btn-active-bg:var(--tblr-vk);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-vk);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-warning {
   *   --tblr-btn-color:var(--tblr-warning);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-warning-fg);
   *   --tblr-btn-hover-bg:var(--tblr-warning);
   *   --tblr-btn-hover-border-color:var(--tblr-warning);
   *   --tblr-btn-active-color:var(--tblr-warning-fg);
   *   --tblr-btn-active-bg:var(--tblr-warning);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-warning);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-yellow {
   *   --tblr-btn-color:var(--tblr-yellow);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-yellow-fg);
   *   --tblr-btn-hover-bg:var(--tblr-yellow);
   *   --tblr-btn-hover-border-color:var(--tblr-yellow);
   *   --tblr-btn-active-color:var(--tblr-yellow-fg);
   *   --tblr-btn-active-bg:var(--tblr-yellow);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-yellow);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-ghost-youtube {
   *   --tblr-btn-color:var(--tblr-youtube);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-youtube-fg);
   *   --tblr-btn-hover-bg:var(--tblr-youtube);
   *   --tblr-btn-hover-border-color:var(--tblr-youtube);
   *   --tblr-btn-active-color:var(--tblr-youtube-fg);
   *   --tblr-btn-active-bg:var(--tblr-youtube);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-active-shadow:inset 0 3px 5px rgba(0,0,0,0.125);
   *   --tblr-btn-disabled-color:var(--tblr-youtube);
   *   --tblr-btn-disabled-bg:transparent;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-gradient:none;
   *   --tblr-btn-box-shadow:none;
   * }
   * }}}
  */
  def `btn-ghost-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-github {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-github-fg);
   *   --tblr-btn-bg:var(--tblr-github);
   *   --tblr-btn-hover-color:var(--tblr-github-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-github-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-github-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-github-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-github);
   *   --tblr-btn-disabled-color:var(--tblr-github-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-google {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-google-fg);
   *   --tblr-btn-bg:var(--tblr-google);
   *   --tblr-btn-hover-color:var(--tblr-google-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-google-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-google-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-google-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-google);
   *   --tblr-btn-disabled-color:var(--tblr-google-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-green {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-green-fg);
   *   --tblr-btn-bg:var(--tblr-green);
   *   --tblr-btn-hover-color:var(--tblr-green-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-green-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-green-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-green-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-green);
   *   --tblr-btn-disabled-color:var(--tblr-green-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group,
   * .btn-group-vertical {
   *   position:relative;
   *   display:inline-flex;
   *   vertical-align:middle;
   * }
   * }}}
  */
  def `btn-group`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group-lg>.btn,
   * .btn-lg {
   *   --tblr-btn-padding-y:0.5rem;
   *   --tblr-btn-padding-x:0.75rem;
   *   --tblr-btn-font-size:1.25rem;
   *   --tblr-btn-border-radius:var(--tblr-border-radius-lg);
   * }
   * }}}
  */
  def `btn-group-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group-sm>.btn,
   * .btn-sm {
   *   --tblr-btn-padding-y:0.125rem;
   *   --tblr-btn-padding-x:0.25rem;
   *   --tblr-btn-font-size:0.75rem;
   *   --tblr-btn-border-radius:var(--tblr-border-radius-sm);
   * }
   * }}}
  */
  def `btn-group-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group,
   * .btn-group-vertical {
   *   position:relative;
   *   display:inline-flex;
   *   vertical-align:middle;
   * }
   * }}}
  */
  def `btn-group-vertical`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-icon {
   *   min-width:calc(var(--tblr-btn-line-height)*var(--tblr-btn-font-size) + var(--tblr-btn-padding-y)*2 + var(--tblr-btn-border-width)*2);
   *   min-height:calc(var(--tblr-btn-line-height)*var(--tblr-btn-font-size) + var(--tblr-btn-padding-y)*2 + var(--tblr-btn-border-width)*2);
   *   padding-left:0;
   *   padding-right:0;
   * }
   * }}}
  */
  def `btn-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-indigo {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-indigo-fg);
   *   --tblr-btn-bg:var(--tblr-indigo);
   *   --tblr-btn-hover-color:var(--tblr-indigo-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-indigo-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-indigo-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-indigo-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-indigo);
   *   --tblr-btn-disabled-color:var(--tblr-indigo-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-info {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-info-fg);
   *   --tblr-btn-bg:var(--tblr-info);
   *   --tblr-btn-hover-color:var(--tblr-info-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-info-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-info-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-info-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-info);
   *   --tblr-btn-disabled-color:var(--tblr-info-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-instagram {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-instagram-fg);
   *   --tblr-btn-bg:var(--tblr-instagram);
   *   --tblr-btn-hover-color:var(--tblr-instagram-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-instagram-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-instagram-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-instagram-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-instagram);
   *   --tblr-btn-disabled-color:var(--tblr-instagram-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group-lg>.btn,
   * .btn-lg {
   *   --tblr-btn-padding-y:0.5rem;
   *   --tblr-btn-padding-x:0.75rem;
   *   --tblr-btn-font-size:1.25rem;
   *   --tblr-btn-border-radius:var(--tblr-border-radius-lg);
   * }
   * }}}
  */
  def `btn-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-light {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-light-fg);
   *   --tblr-btn-bg:var(--tblr-light);
   *   --tblr-btn-hover-color:var(--tblr-light-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-light-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-light-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-light-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-light);
   *   --tblr-btn-disabled-color:var(--tblr-light-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-lime {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-lime-fg);
   *   --tblr-btn-bg:var(--tblr-lime);
   *   --tblr-btn-hover-color:var(--tblr-lime-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-lime-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-lime-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-lime-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-lime);
   *   --tblr-btn-disabled-color:var(--tblr-lime-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-link {
   *   --tblr-btn-font-weight:400;
   *   --tblr-btn-color:var(--tblr-link-color);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-color:var(--tblr-link-hover-color);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-color:var(--tblr-link-hover-color);
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-disabled-color:#667382;
   *   --tblr-btn-disabled-border-color:transparent;
   *   --tblr-btn-box-shadow:0 0 0 #000;
   *   --tblr-btn-focus-shadow-rgb:38,109,179;
   *   text-decoration:none;
   * }
   * }}}
  */
  def `btn-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-linkedin {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-bg:var(--tblr-linkedin);
   *   --tblr-btn-hover-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-linkedin-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-linkedin-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-linkedin);
   *   --tblr-btn-disabled-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-list {
   *   --tblr-list-gap:0.5rem;
   *   display:flex;
   *   flex-wrap:wrap;
   *   gap:var(--tblr-list-gap);
   * }
   * }}}
  */
  def `btn-list`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-loading {
   *   position:relative;
   *   color:transparent !important;
   *   text-shadow:none !important;
   *   pointer-events:none;
   * }
   * }}}
  */
  def `btn-loading`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-muted {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-muted-fg);
   *   --tblr-btn-bg:var(--tblr-muted);
   *   --tblr-btn-hover-color:var(--tblr-muted-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-muted-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-muted-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-muted-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-muted);
   *   --tblr-btn-disabled-color:var(--tblr-muted-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-orange {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-orange-fg);
   *   --tblr-btn-bg:var(--tblr-orange);
   *   --tblr-btn-hover-color:var(--tblr-orange-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-orange-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-orange-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-orange-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-orange);
   *   --tblr-btn-disabled-color:var(--tblr-orange-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-azure {
   *   --tblr-btn-color:var(--tblr-azure);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-azure);
   *   --tblr-btn-hover-color:var(--tblr-azure-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-azure);
   *   --tblr-btn-active-color:var(--tblr-azure-fg);
   *   --tblr-btn-active-bg:var(--tblr-azure);
   *   --tblr-btn-disabled-color:var(--tblr-azure);
   *   --tblr-btn-disabled-border-color:var(--tblr-azure);
   * }
   * }}}
  */
  def `btn-outline-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-bitbucket {
   *   --tblr-btn-color:var(--tblr-bitbucket);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-bitbucket);
   *   --tblr-btn-hover-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-bitbucket);
   *   --tblr-btn-active-color:var(--tblr-bitbucket-fg);
   *   --tblr-btn-active-bg:var(--tblr-bitbucket);
   *   --tblr-btn-disabled-color:var(--tblr-bitbucket);
   *   --tblr-btn-disabled-border-color:var(--tblr-bitbucket);
   * }
   * }}}
  */
  def `btn-outline-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-blue {
   *   --tblr-btn-color:var(--tblr-blue);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-blue);
   *   --tblr-btn-hover-color:var(--tblr-blue-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-blue);
   *   --tblr-btn-active-color:var(--tblr-blue-fg);
   *   --tblr-btn-active-bg:var(--tblr-blue);
   *   --tblr-btn-disabled-color:var(--tblr-blue);
   *   --tblr-btn-disabled-border-color:var(--tblr-blue);
   * }
   * }}}
  */
  def `btn-outline-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-cyan {
   *   --tblr-btn-color:var(--tblr-cyan);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-cyan);
   *   --tblr-btn-hover-color:var(--tblr-cyan-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-cyan);
   *   --tblr-btn-active-color:var(--tblr-cyan-fg);
   *   --tblr-btn-active-bg:var(--tblr-cyan);
   *   --tblr-btn-disabled-color:var(--tblr-cyan);
   *   --tblr-btn-disabled-border-color:var(--tblr-cyan);
   * }
   * }}}
  */
  def `btn-outline-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-danger {
   *   --tblr-btn-color:var(--tblr-danger);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-danger);
   *   --tblr-btn-hover-color:var(--tblr-danger-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-danger);
   *   --tblr-btn-active-color:var(--tblr-danger-fg);
   *   --tblr-btn-active-bg:var(--tblr-danger);
   *   --tblr-btn-disabled-color:var(--tblr-danger);
   *   --tblr-btn-disabled-border-color:var(--tblr-danger);
   * }
   * }}}
  */
  def `btn-outline-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-dark {
   *   --tblr-btn-color:var(--tblr-dark);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-dark);
   *   --tblr-btn-hover-color:var(--tblr-dark-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-dark);
   *   --tblr-btn-active-color:var(--tblr-dark-fg);
   *   --tblr-btn-active-bg:var(--tblr-dark);
   *   --tblr-btn-disabled-color:var(--tblr-dark);
   *   --tblr-btn-disabled-border-color:var(--tblr-dark);
   * }
   * }}}
  */
  def `btn-outline-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-dribbble {
   *   --tblr-btn-color:var(--tblr-dribbble);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-dribbble);
   *   --tblr-btn-hover-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-dribbble);
   *   --tblr-btn-active-color:var(--tblr-dribbble-fg);
   *   --tblr-btn-active-bg:var(--tblr-dribbble);
   *   --tblr-btn-disabled-color:var(--tblr-dribbble);
   *   --tblr-btn-disabled-border-color:var(--tblr-dribbble);
   * }
   * }}}
  */
  def `btn-outline-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-facebook {
   *   --tblr-btn-color:var(--tblr-facebook);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-facebook);
   *   --tblr-btn-hover-color:var(--tblr-facebook-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-facebook);
   *   --tblr-btn-active-color:var(--tblr-facebook-fg);
   *   --tblr-btn-active-bg:var(--tblr-facebook);
   *   --tblr-btn-disabled-color:var(--tblr-facebook);
   *   --tblr-btn-disabled-border-color:var(--tblr-facebook);
   * }
   * }}}
  */
  def `btn-outline-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-flickr {
   *   --tblr-btn-color:var(--tblr-flickr);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-flickr);
   *   --tblr-btn-hover-color:var(--tblr-flickr-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-flickr);
   *   --tblr-btn-active-color:var(--tblr-flickr-fg);
   *   --tblr-btn-active-bg:var(--tblr-flickr);
   *   --tblr-btn-disabled-color:var(--tblr-flickr);
   *   --tblr-btn-disabled-border-color:var(--tblr-flickr);
   * }
   * }}}
  */
  def `btn-outline-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-github {
   *   --tblr-btn-color:var(--tblr-github);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-github);
   *   --tblr-btn-hover-color:var(--tblr-github-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-github);
   *   --tblr-btn-active-color:var(--tblr-github-fg);
   *   --tblr-btn-active-bg:var(--tblr-github);
   *   --tblr-btn-disabled-color:var(--tblr-github);
   *   --tblr-btn-disabled-border-color:var(--tblr-github);
   * }
   * }}}
  */
  def `btn-outline-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-google {
   *   --tblr-btn-color:var(--tblr-google);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-google);
   *   --tblr-btn-hover-color:var(--tblr-google-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-google);
   *   --tblr-btn-active-color:var(--tblr-google-fg);
   *   --tblr-btn-active-bg:var(--tblr-google);
   *   --tblr-btn-disabled-color:var(--tblr-google);
   *   --tblr-btn-disabled-border-color:var(--tblr-google);
   * }
   * }}}
  */
  def `btn-outline-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-green {
   *   --tblr-btn-color:var(--tblr-green);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-green);
   *   --tblr-btn-hover-color:var(--tblr-green-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-green);
   *   --tblr-btn-active-color:var(--tblr-green-fg);
   *   --tblr-btn-active-bg:var(--tblr-green);
   *   --tblr-btn-disabled-color:var(--tblr-green);
   *   --tblr-btn-disabled-border-color:var(--tblr-green);
   * }
   * }}}
  */
  def `btn-outline-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-indigo {
   *   --tblr-btn-color:var(--tblr-indigo);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-indigo);
   *   --tblr-btn-hover-color:var(--tblr-indigo-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-indigo);
   *   --tblr-btn-active-color:var(--tblr-indigo-fg);
   *   --tblr-btn-active-bg:var(--tblr-indigo);
   *   --tblr-btn-disabled-color:var(--tblr-indigo);
   *   --tblr-btn-disabled-border-color:var(--tblr-indigo);
   * }
   * }}}
  */
  def `btn-outline-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-info {
   *   --tblr-btn-color:var(--tblr-info);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-info);
   *   --tblr-btn-hover-color:var(--tblr-info-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-info);
   *   --tblr-btn-active-color:var(--tblr-info-fg);
   *   --tblr-btn-active-bg:var(--tblr-info);
   *   --tblr-btn-disabled-color:var(--tblr-info);
   *   --tblr-btn-disabled-border-color:var(--tblr-info);
   * }
   * }}}
  */
  def `btn-outline-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-instagram {
   *   --tblr-btn-color:var(--tblr-instagram);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-instagram);
   *   --tblr-btn-hover-color:var(--tblr-instagram-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-instagram);
   *   --tblr-btn-active-color:var(--tblr-instagram-fg);
   *   --tblr-btn-active-bg:var(--tblr-instagram);
   *   --tblr-btn-disabled-color:var(--tblr-instagram);
   *   --tblr-btn-disabled-border-color:var(--tblr-instagram);
   * }
   * }}}
  */
  def `btn-outline-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-light {
   *   --tblr-btn-color:var(--tblr-light);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-light);
   *   --tblr-btn-hover-color:var(--tblr-light-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-light);
   *   --tblr-btn-active-color:var(--tblr-light-fg);
   *   --tblr-btn-active-bg:var(--tblr-light);
   *   --tblr-btn-disabled-color:var(--tblr-light);
   *   --tblr-btn-disabled-border-color:var(--tblr-light);
   * }
   * }}}
  */
  def `btn-outline-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-lime {
   *   --tblr-btn-color:var(--tblr-lime);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-lime);
   *   --tblr-btn-hover-color:var(--tblr-lime-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-lime);
   *   --tblr-btn-active-color:var(--tblr-lime-fg);
   *   --tblr-btn-active-bg:var(--tblr-lime);
   *   --tblr-btn-disabled-color:var(--tblr-lime);
   *   --tblr-btn-disabled-border-color:var(--tblr-lime);
   * }
   * }}}
  */
  def `btn-outline-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-linkedin {
   *   --tblr-btn-color:var(--tblr-linkedin);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-linkedin);
   *   --tblr-btn-hover-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-linkedin);
   *   --tblr-btn-active-color:var(--tblr-linkedin-fg);
   *   --tblr-btn-active-bg:var(--tblr-linkedin);
   *   --tblr-btn-disabled-color:var(--tblr-linkedin);
   *   --tblr-btn-disabled-border-color:var(--tblr-linkedin);
   * }
   * }}}
  */
  def `btn-outline-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-muted {
   *   --tblr-btn-color:var(--tblr-muted);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-muted);
   *   --tblr-btn-hover-color:var(--tblr-muted-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-muted);
   *   --tblr-btn-active-color:var(--tblr-muted-fg);
   *   --tblr-btn-active-bg:var(--tblr-muted);
   *   --tblr-btn-disabled-color:var(--tblr-muted);
   *   --tblr-btn-disabled-border-color:var(--tblr-muted);
   * }
   * }}}
  */
  def `btn-outline-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-orange {
   *   --tblr-btn-color:var(--tblr-orange);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-orange);
   *   --tblr-btn-hover-color:var(--tblr-orange-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-orange);
   *   --tblr-btn-active-color:var(--tblr-orange-fg);
   *   --tblr-btn-active-bg:var(--tblr-orange);
   *   --tblr-btn-disabled-color:var(--tblr-orange);
   *   --tblr-btn-disabled-border-color:var(--tblr-orange);
   * }
   * }}}
  */
  def `btn-outline-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-pink {
   *   --tblr-btn-color:var(--tblr-pink);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-pink);
   *   --tblr-btn-hover-color:var(--tblr-pink-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-pink);
   *   --tblr-btn-active-color:var(--tblr-pink-fg);
   *   --tblr-btn-active-bg:var(--tblr-pink);
   *   --tblr-btn-disabled-color:var(--tblr-pink);
   *   --tblr-btn-disabled-border-color:var(--tblr-pink);
   * }
   * }}}
  */
  def `btn-outline-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-pinterest {
   *   --tblr-btn-color:var(--tblr-pinterest);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-pinterest);
   *   --tblr-btn-hover-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-pinterest);
   *   --tblr-btn-active-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-active-bg:var(--tblr-pinterest);
   *   --tblr-btn-disabled-color:var(--tblr-pinterest);
   *   --tblr-btn-disabled-border-color:var(--tblr-pinterest);
   * }
   * }}}
  */
  def `btn-outline-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-primary {
   *   --tblr-btn-color:var(--tblr-primary);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-primary);
   *   --tblr-btn-hover-color:var(--tblr-primary-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-primary);
   *   --tblr-btn-active-color:var(--tblr-primary-fg);
   *   --tblr-btn-active-bg:var(--tblr-primary);
   *   --tblr-btn-disabled-color:var(--tblr-primary);
   *   --tblr-btn-disabled-border-color:var(--tblr-primary);
   * }
   * }}}
  */
  def `btn-outline-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-purple {
   *   --tblr-btn-color:var(--tblr-purple);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-purple);
   *   --tblr-btn-hover-color:var(--tblr-purple-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-purple);
   *   --tblr-btn-active-color:var(--tblr-purple-fg);
   *   --tblr-btn-active-bg:var(--tblr-purple);
   *   --tblr-btn-disabled-color:var(--tblr-purple);
   *   --tblr-btn-disabled-border-color:var(--tblr-purple);
   * }
   * }}}
  */
  def `btn-outline-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-red {
   *   --tblr-btn-color:var(--tblr-red);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-red);
   *   --tblr-btn-hover-color:var(--tblr-red-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-red);
   *   --tblr-btn-active-color:var(--tblr-red-fg);
   *   --tblr-btn-active-bg:var(--tblr-red);
   *   --tblr-btn-disabled-color:var(--tblr-red);
   *   --tblr-btn-disabled-border-color:var(--tblr-red);
   * }
   * }}}
  */
  def `btn-outline-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-rss {
   *   --tblr-btn-color:var(--tblr-rss);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-rss);
   *   --tblr-btn-hover-color:var(--tblr-rss-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-rss);
   *   --tblr-btn-active-color:var(--tblr-rss-fg);
   *   --tblr-btn-active-bg:var(--tblr-rss);
   *   --tblr-btn-disabled-color:var(--tblr-rss);
   *   --tblr-btn-disabled-border-color:var(--tblr-rss);
   * }
   * }}}
  */
  def `btn-outline-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-secondary {
   *   --tblr-btn-color:var(--tblr-secondary);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-secondary);
   *   --tblr-btn-hover-color:var(--tblr-secondary-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-secondary);
   *   --tblr-btn-active-color:var(--tblr-secondary-fg);
   *   --tblr-btn-active-bg:var(--tblr-secondary);
   *   --tblr-btn-disabled-color:var(--tblr-secondary);
   *   --tblr-btn-disabled-border-color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `btn-outline-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-success {
   *   --tblr-btn-color:var(--tblr-success);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-success);
   *   --tblr-btn-hover-color:var(--tblr-success-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-success);
   *   --tblr-btn-active-color:var(--tblr-success-fg);
   *   --tblr-btn-active-bg:var(--tblr-success);
   *   --tblr-btn-disabled-color:var(--tblr-success);
   *   --tblr-btn-disabled-border-color:var(--tblr-success);
   * }
   * }}}
  */
  def `btn-outline-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-tabler {
   *   --tblr-btn-color:var(--tblr-tabler);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-tabler);
   *   --tblr-btn-hover-color:var(--tblr-tabler-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-tabler);
   *   --tblr-btn-active-color:var(--tblr-tabler-fg);
   *   --tblr-btn-active-bg:var(--tblr-tabler);
   *   --tblr-btn-disabled-color:var(--tblr-tabler);
   *   --tblr-btn-disabled-border-color:var(--tblr-tabler);
   * }
   * }}}
  */
  def `btn-outline-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-teal {
   *   --tblr-btn-color:var(--tblr-teal);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-teal);
   *   --tblr-btn-hover-color:var(--tblr-teal-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-teal);
   *   --tblr-btn-active-color:var(--tblr-teal-fg);
   *   --tblr-btn-active-bg:var(--tblr-teal);
   *   --tblr-btn-disabled-color:var(--tblr-teal);
   *   --tblr-btn-disabled-border-color:var(--tblr-teal);
   * }
   * }}}
  */
  def `btn-outline-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-twitter {
   *   --tblr-btn-color:var(--tblr-twitter);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-twitter);
   *   --tblr-btn-hover-color:var(--tblr-twitter-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-twitter);
   *   --tblr-btn-active-color:var(--tblr-twitter-fg);
   *   --tblr-btn-active-bg:var(--tblr-twitter);
   *   --tblr-btn-disabled-color:var(--tblr-twitter);
   *   --tblr-btn-disabled-border-color:var(--tblr-twitter);
   * }
   * }}}
  */
  def `btn-outline-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-vimeo {
   *   --tblr-btn-color:var(--tblr-vimeo);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-vimeo);
   *   --tblr-btn-hover-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-vimeo);
   *   --tblr-btn-active-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-active-bg:var(--tblr-vimeo);
   *   --tblr-btn-disabled-color:var(--tblr-vimeo);
   *   --tblr-btn-disabled-border-color:var(--tblr-vimeo);
   * }
   * }}}
  */
  def `btn-outline-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-vk {
   *   --tblr-btn-color:var(--tblr-vk);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-vk);
   *   --tblr-btn-hover-color:var(--tblr-vk-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-vk);
   *   --tblr-btn-active-color:var(--tblr-vk-fg);
   *   --tblr-btn-active-bg:var(--tblr-vk);
   *   --tblr-btn-disabled-color:var(--tblr-vk);
   *   --tblr-btn-disabled-border-color:var(--tblr-vk);
   * }
   * }}}
  */
  def `btn-outline-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-warning {
   *   --tblr-btn-color:var(--tblr-warning);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-warning);
   *   --tblr-btn-hover-color:var(--tblr-warning-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-warning);
   *   --tblr-btn-active-color:var(--tblr-warning-fg);
   *   --tblr-btn-active-bg:var(--tblr-warning);
   *   --tblr-btn-disabled-color:var(--tblr-warning);
   *   --tblr-btn-disabled-border-color:var(--tblr-warning);
   * }
   * }}}
  */
  def `btn-outline-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-yellow {
   *   --tblr-btn-color:var(--tblr-yellow);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-yellow);
   *   --tblr-btn-hover-color:var(--tblr-yellow-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-yellow);
   *   --tblr-btn-active-color:var(--tblr-yellow-fg);
   *   --tblr-btn-active-bg:var(--tblr-yellow);
   *   --tblr-btn-disabled-color:var(--tblr-yellow);
   *   --tblr-btn-disabled-border-color:var(--tblr-yellow);
   * }
   * }}}
  */
  def `btn-outline-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-outline-youtube {
   *   --tblr-btn-color:var(--tblr-youtube);
   *   --tblr-btn-bg:transparent;
   *   --tblr-btn-border-color:var(--tblr-youtube);
   *   --tblr-btn-hover-color:var(--tblr-youtube-fg);
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-hover-bg:var(--tblr-youtube);
   *   --tblr-btn-active-color:var(--tblr-youtube-fg);
   *   --tblr-btn-active-bg:var(--tblr-youtube);
   *   --tblr-btn-disabled-color:var(--tblr-youtube);
   *   --tblr-btn-disabled-border-color:var(--tblr-youtube);
   * }
   * }}}
  */
  def `btn-outline-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-pill {
   *   padding-right:1.5em;
   *   padding-left:1.5em;
   *   border-radius:10rem;
   * }
   * }}}
  */
  def `btn-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-pink {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-pink-fg);
   *   --tblr-btn-bg:var(--tblr-pink);
   *   --tblr-btn-hover-color:var(--tblr-pink-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-pink-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-pink-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-pink-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-pink);
   *   --tblr-btn-disabled-color:var(--tblr-pink-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-pinterest {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-bg:var(--tblr-pinterest);
   *   --tblr-btn-hover-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-pinterest-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-pinterest-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-pinterest);
   *   --tblr-btn-disabled-color:var(--tblr-pinterest-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-primary {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-primary-fg);
   *   --tblr-btn-bg:var(--tblr-primary);
   *   --tblr-btn-hover-color:var(--tblr-primary-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-primary-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-primary-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-primary-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-primary);
   *   --tblr-btn-disabled-color:var(--tblr-primary-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-purple {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-purple-fg);
   *   --tblr-btn-bg:var(--tblr-purple);
   *   --tblr-btn-hover-color:var(--tblr-purple-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-purple-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-purple-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-purple-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-purple);
   *   --tblr-btn-disabled-color:var(--tblr-purple-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-red {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-red-fg);
   *   --tblr-btn-bg:var(--tblr-red);
   *   --tblr-btn-hover-color:var(--tblr-red-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-red-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-red-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-red-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-red);
   *   --tblr-btn-disabled-color:var(--tblr-red-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-rss {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-rss-fg);
   *   --tblr-btn-bg:var(--tblr-rss);
   *   --tblr-btn-hover-color:var(--tblr-rss-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-rss-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-rss-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-rss-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-rss);
   *   --tblr-btn-disabled-color:var(--tblr-rss-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-secondary {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-secondary-fg);
   *   --tblr-btn-bg:var(--tblr-secondary);
   *   --tblr-btn-hover-color:var(--tblr-secondary-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-secondary-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-secondary-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-secondary-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-secondary);
   *   --tblr-btn-disabled-color:var(--tblr-secondary-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group-sm>.btn,
   * .btn-sm {
   *   --tblr-btn-padding-y:0.125rem;
   *   --tblr-btn-padding-x:0.25rem;
   *   --tblr-btn-font-size:0.75rem;
   *   --tblr-btn-border-radius:var(--tblr-border-radius-sm);
   * }
   * }}}
  */
  def `btn-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-square { border-radius:0; }
   * }}}
  */
  def `btn-square`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-success {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-success-fg);
   *   --tblr-btn-bg:var(--tblr-success);
   *   --tblr-btn-hover-color:var(--tblr-success-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-success-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-success-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-success-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-success);
   *   --tblr-btn-disabled-color:var(--tblr-success-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-tabler {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-tabler-fg);
   *   --tblr-btn-bg:var(--tblr-tabler);
   *   --tblr-btn-hover-color:var(--tblr-tabler-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-tabler-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-tabler-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-tabler-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-tabler);
   *   --tblr-btn-disabled-color:var(--tblr-tabler-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-teal {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-teal-fg);
   *   --tblr-btn-bg:var(--tblr-teal);
   *   --tblr-btn-hover-color:var(--tblr-teal-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-teal-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-teal-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-teal-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-teal);
   *   --tblr-btn-disabled-color:var(--tblr-teal-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-toolbar {
   *   display:flex;
   *   flex-wrap:wrap;
   *   justify-content:flex-start;
   * }
   * }}}
  */
  def `btn-toolbar`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-twitter {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-twitter-fg);
   *   --tblr-btn-bg:var(--tblr-twitter);
   *   --tblr-btn-hover-color:var(--tblr-twitter-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-twitter-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-twitter-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-twitter-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-twitter);
   *   --tblr-btn-disabled-color:var(--tblr-twitter-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-vimeo {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-bg:var(--tblr-vimeo);
   *   --tblr-btn-hover-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-vimeo-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-vimeo-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-vimeo);
   *   --tblr-btn-disabled-color:var(--tblr-vimeo-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-vk {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-vk-fg);
   *   --tblr-btn-bg:var(--tblr-vk);
   *   --tblr-btn-hover-color:var(--tblr-vk-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-vk-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-vk-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-vk-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-vk);
   *   --tblr-btn-disabled-color:var(--tblr-vk-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-warning {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-warning-fg);
   *   --tblr-btn-bg:var(--tblr-warning);
   *   --tblr-btn-hover-color:var(--tblr-warning-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-warning-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-warning-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-warning-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-warning);
   *   --tblr-btn-disabled-color:var(--tblr-warning-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-yellow {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-yellow-fg);
   *   --tblr-btn-bg:var(--tblr-yellow);
   *   --tblr-btn-hover-color:var(--tblr-yellow-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-yellow-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-yellow-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-yellow-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-yellow);
   *   --tblr-btn-disabled-color:var(--tblr-yellow-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-youtube {
   *   --tblr-btn-border-color:transparent;
   *   --tblr-btn-hover-border-color:transparent;
   *   --tblr-btn-active-border-color:transparent;
   *   --tblr-btn-color:var(--tblr-youtube-fg);
   *   --tblr-btn-bg:var(--tblr-youtube);
   *   --tblr-btn-hover-color:var(--tblr-youtube-fg);
   *   --tblr-btn-hover-bg:rgba(var(--tblr-youtube-rgb),.8);
   *   --tblr-btn-active-color:var(--tblr-youtube-fg);
   *   --tblr-btn-active-bg:rgba(var(--tblr-youtube-rgb),.8);
   *   --tblr-btn-disabled-bg:var(--tblr-youtube);
   *   --tblr-btn-disabled-color:var(--tblr-youtube-fg);
   *   --tblr-btn-box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `btn-youtube`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__months .button-next-month,
   * .litepicker .container__months .button-prev-month { cursor:pointer !important; }
   * }}}
  */
  def `button-next-month`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__months .button-next-month,
   * .litepicker .container__months .button-prev-month { cursor:pointer !important; }
   * }}}
  */
  def `button-prev-month`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .c,
   * .highlight .c1 { color:#a0aec0; }
   * }}}
  */
  def c: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .c,
   * .highlight .c1 { color:#a0aec0; }
   * }}}
  */
  def c1: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar {
   *   display:block;
   *   font-size:.765625rem;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def calendar: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-body,
   * .calendar-header {
   *   display:flex;
   *   flex-wrap:wrap;
   *   justify-content:flex-start;
   *   padding:.5rem 0;
   * }
   * }}}
  */
  def `calendar-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-date {
   *   flex:0 0 14.2857142857%;
   *   max-width:14.2857142857%;
   *   padding:.2rem;
   *   text-align:center;
   *   border:0;
   * }
   * }}}
  */
  def `calendar-date`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-body,
   * .calendar-header {
   *   display:flex;
   *   flex-wrap:wrap;
   *   justify-content:flex-start;
   *   padding:.5rem 0;
   * }
   * }}}
  */
  def `calendar-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-nav {
   *   display:flex;
   *   align-items:center;
   * }
   * }}}
  */
  def `calendar-nav`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-range { position:relative; }
   * }}}
  */
  def `calendar-range`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-title {
   *   flex:1;
   *   text-align:center;
   * }
   * }}}
  */
  def `calendar-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .caption-top { caption-side:top; }
   * }}}
  */
  def `caption-top`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .example-column>.card:last-of-type { margin-bottom:0; }
   * }}}
  */
  def card: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-actions {
   *   margin:-.5rem -.5rem -.5rem auto;
   *   padding-left:.5rem;
   * }
   * }}}
  */
  def `card-actions`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-active {
   *   --tblr-card-border-color:var(--tblr-primary);
   *   --tblr-card-bg:var(--tblr-active-bg);
   * }
   * }}}
  */
  def `card-active`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-avatar {
   *   margin-left:auto;
   *   margin-right:auto;
   *   box-shadow:0 0 0 .25rem var(--tblr-card-bg,var(--tblr-bg-surface));
   *   margin-top:calc(-1*var(--tblr-avatar-size)*.5);
   * }
   * }}}
  */
  def `card-avatar`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-body {
   *   flex:1 1 auto;
   *   padding:var(--tblr-card-spacer-y) var(--tblr-card-spacer-x);
   *   color:var(--tblr-card-color);
   * }
   * }}}
  */
  def `card-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-body-scrollable { overflow:auto; }
   * }}}
  */
  def `card-body-scrollable`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-borderless,
   * .card-borderless .card-footer,
   * .card-borderless .card-header { border-color:transparent; }
   * }}}
  */
  def `card-borderless`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-btn {
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   padding:1.25rem 1.25rem;
   *   text-align:center;
   *   transition:background .3s;
   *   border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   flex:1;
   *   color:inherit;
   *   font-weight:var(--tblr-font-weight-medium);
   * }
   * }}}
  */
  def `card-btn`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-chart {
   *   position:relative;
   *   z-index:1;
   *   height:3.5rem;
   * }
   * }}}
  */
  def `card-chart`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-code { padding:0; }
   * }}}
  */
  def `card-code`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-cover {
   *   position:relative;
   *   padding:1.25rem 1.25rem;
   *   background:#666 no-repeat center/cover;
   * }
   * }}}
  */
  def `card-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-cover-blurred:before {
   *   -webkit-backdrop-filter:blur(2px);
   *   backdrop-filter:blur(2px);
   * }
   * }}}
  */
  def `card-cover-blurred`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card>.card-header+.list-group,
   * .card>.list-group+.card-footer { border-top:0; }
   * }}}
  */
  def `card-footer`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-footer-borderless { border-top:none; }
   * }}}
  */
  def `card-footer-borderless`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-footer-transparent {
   *   background:0 0;
   *   border-color:transparent;
   *   padding-top:0;
   * }
   * }}}
  */
  def `card-footer-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-group>.card { margin-bottom:var(--tblr-card-group-margin); }
   * }}}
  */
  def `card-group`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card>.card-header+.list-group,
   * .card>.list-group+.card-footer { border-top:0; }
   * }}}
  */
  def `card-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-header-light {
   *   border-bottom-color:transparent;
   *   background:var(--tblr-bg-surface-tertiary);
   * }
   * }}}
  */
  def `card-header-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-header-pills {
   *   margin-right:calc(-.5*var(--tblr-card-cap-padding-x));
   *   margin-left:calc(-.5*var(--tblr-card-cap-padding-x));
   * }
   * }}}
  */
  def `card-header-pills`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-header-tabs {
   *   margin-right:calc(-.5*var(--tblr-card-cap-padding-x));
   *   margin-bottom:calc(-1*var(--tblr-card-cap-padding-y));
   *   margin-left:calc(-.5*var(--tblr-card-cap-padding-x));
   *   border-bottom:0;
   * }
   * }}}
  */
  def `card-header-tabs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img,
   * .card-img-bottom,
   * .card-img-top { width:100%; }
   * }}}
  */
  def `card-img`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img,
   * .card-img-bottom,
   * .card-img-top { width:100%; }
   * }}}
  */
  def `card-img-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img,
   * .card-img-end {
   *   border-top-right-radius:calc(var(--tblr-border-radius) - (var(--tblr-border-width)));
   *   border-bottom-right-radius:calc(var(--tblr-border-radius) - (var(--tblr-border-width)));
   * }
   * }}}
  */
  def `card-img-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img-overlay {
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   bottom:0;
   *   left:0;
   *   padding:var(--tblr-card-img-overlay-padding);
   *   border-radius:var(--tblr-card-inner-border-radius);
   * }
   * }}}
  */
  def `card-img-overlay`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img-overlay-dark { background-image:linear-gradient(180deg,rgba(0,0,0,0) 0,rgba(0,0,0,.6) 100%); }
   * }}}
  */
  def `card-img-overlay-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img,
   * .card-img-start {
   *   border-top-left-radius:calc(var(--tblr-border-radius) - (var(--tblr-border-width)));
   *   border-bottom-left-radius:calc(var(--tblr-border-radius) - (var(--tblr-border-width)));
   * }
   * }}}
  */
  def `card-img-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-img,
   * .card-img-bottom,
   * .card-img-top { width:100%; }
   * }}}
  */
  def `card-img-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-inactive {
   *   pointer-events:none;
   *   box-shadow:none;
   * }
   * }}}
  */
  def `card-inactive`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-lg>.card-body { padding:2rem; }
   * }}}
  */
  def `card-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-link:hover { text-decoration:none; }
   * }}}
  */
  def `card-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-link-pop:hover {
   *   transform:translateY(-2px);
   *   opacity:1;
   * }
   * }}}
  */
  def `card-link-pop`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-link-rotate:hover {
   *   transform:rotate(1.5deg);
   *   opacity:1;
   * }
   * }}}
  */
  def `card-link-rotate`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-body+.card-list-group { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color); }
   * }}}
  */
  def `card-list-group`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-md>.card-body { padding:2.5rem; }
   * }}}
  */
  def `card-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-meta { color:var(--tblr-secondary); }
   * }}}
  */
  def `card-meta`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-note {
   *   --tblr-card-bg:#fff7dd;
   *   --tblr-card-border-color:#fff1c9;
   * }
   * }}}
  */
  def `card-note`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-options {
   *   top:1.5rem;
   *   right:.75rem;
   *   display:flex;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `card-options`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-options-link {
   *   display:inline-block;
   *   min-width:1rem;
   *   margin-left:.25rem;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `card-options-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-progress { height:.25rem; }
   * }}}
  */
  def `card-progress`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-rotate-left { transform:rotate(-1.5deg); }
   * }}}
  */
  def `card-rotate-left`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-rotate-right { transform:rotate(1.5deg); }
   * }}}
  */
  def `card-rotate-right`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-sm>.card-body { padding:1rem; }
   * }}}
  */
  def `card-sm`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .card-sponsor {
   *   background:var(--tblr-primary-lt) no-repeat center/100% 100%;
   *   border-color:var(--tblr-primary);
   *   min-height:316px;
   * }
   * }}}
  */
  def `card-sponsor`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-stacked {
   *   --tblr-card-stacked-offset:.25rem;
   *   position:relative;
   * }
   * }}}
  */
  def `card-stacked`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-stamp {
   *   --tblr-stamp-size:7rem;
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   width:calc(var(--tblr-stamp-size)*1);
   *   height:calc(var(--tblr-stamp-size)*1);
   *   max-height:100%;
   *   border-top-right-radius:4px;
   *   opacity:.2;
   *   overflow:hidden;
   *   pointer-events:none;
   * }
   * }}}
  */
  def `card-stamp`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-stamp-icon {
   *   background:var(--tblr-secondary);
   *   color:var(--tblr-card-bg,var(--tblr-bg-surface));
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   border-radius:100rem;
   *   width:calc(var(--tblr-stamp-size)*1);
   *   height:calc(var(--tblr-stamp-size)*1);
   *   position:relative;
   *   top:calc(var(--tblr-stamp-size)*-.25);
   *   right:calc(var(--tblr-stamp-size)*-.25);
   *   font-size:calc(var(--tblr-stamp-size)*.75);
   *   transform:rotate(10deg);
   * }
   * }}}
  */
  def `card-stamp-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-stamp-lg { --tblr-stamp-size:13rem; }
   * }}}
  */
  def `card-stamp-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-status-bottom {
   *   position:absolute;
   *   top:initial;
   *   bottom:0;
   *   width:100%;
   *   height:2px;
   *   border-radius:0 0 var(--tblr-card-border-radius) var(--tblr-card-border-radius);
   * }
   * }}}
  */
  def `card-status-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-status-start {
   *   position:absolute;
   *   right:auto;
   *   bottom:0;
   *   width:2px;
   *   height:100%;
   *   border-radius:var(--tblr-card-border-radius) 0 0 var(--tblr-card-border-radius);
   * }
   * }}}
  */
  def `card-status-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-status-top {
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   left:0;
   *   height:2px;
   *   border-radius:var(--tblr-card-border-radius) var(--tblr-card-border-radius) 0 0;
   * }
   * }}}
  */
  def `card-status-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-subtitle {
   *   margin-top:calc(-.5*var(--tblr-card-title-spacer-y));
   *   margin-bottom:0;
   *   color:var(--tblr-card-subtitle-color);
   * }
   * }}}
  */
  def `card-subtitle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-table { margin-bottom:0 !important; }
   * }}}
  */
  def `card-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-tabs .nav-tabs {
   *   position:relative;
   *   z-index:1000;
   *   border-bottom:0;
   * }
   * }}}
  */
  def `card-tabs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-text:last-child { margin-bottom:0; }
   * }}}
  */
  def `card-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-title {
   *   margin-bottom:var(--tblr-card-title-spacer-y);
   *   color:var(--tblr-card-title-color);
   * }
   * }}}
  */
  def `card-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel { position:relative; }
   * }}}
  */
  def carousel: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-caption {
   *   position:absolute;
   *   right:15%;
   *   bottom:1.25rem;
   *   left:15%;
   *   padding-top:1.25rem;
   *   padding-bottom:1.25rem;
   *   color:#fff;
   *   text-align:center;
   * }
   * }}}
  */
  def `carousel-caption`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-caption-background {
   *   background:red;
   *   position:absolute;
   *   left:0;
   *   right:0;
   *   bottom:0;
   *   height:90%;
   *   background:linear-gradient(0deg,rgba(24,36,51,.9),rgba(24,36,51,0));
   * }
   * }}}
  */
  def `carousel-caption-background`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-control-next,
   * .carousel-control-prev {
   *   position:absolute;
   *   top:0;
   *   bottom:0;
   *   z-index:1;
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   width:15%;
   *   padding:0;
   *   color:#fff;
   *   text-align:center;
   *   background:0 0;
   *   border:0;
   *   opacity:.5;
   *   transition:opacity .15s ease;
   * }
   * }}}
  */
  def `carousel-control-next`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-control-next-icon,
   * .carousel-control-prev-icon {
   *   display:inline-block;
   *   width:1.5rem;
   *   height:1.5rem;
   *   background-repeat:no-repeat;
   *   background-position:50%;
   *   background-size:100% 100%;
   * }
   * }}}
  */
  def `carousel-control-next-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-control-next,
   * .carousel-control-prev {
   *   position:absolute;
   *   top:0;
   *   bottom:0;
   *   z-index:1;
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   width:15%;
   *   padding:0;
   *   color:#fff;
   *   text-align:center;
   *   background:0 0;
   *   border:0;
   *   opacity:.5;
   *   transition:opacity .15s ease;
   * }
   * }}}
  */
  def `carousel-control-prev`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-control-next-icon,
   * .carousel-control-prev-icon {
   *   display:inline-block;
   *   width:1.5rem;
   *   height:1.5rem;
   *   background-repeat:no-repeat;
   *   background-position:50%;
   *   background-size:100% 100%;
   * }
   * }}}
  */
  def `carousel-control-prev-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-dark .carousel-control-next-icon,
   * .carousel-dark .carousel-control-prev-icon { filter:invert(1) grayscale(100); }
   * }}}
  */
  def `carousel-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-fade .carousel-item {
   *   opacity:0;
   *   transition-property:opacity;
   *   transform:none;
   * }
   * }}}
  */
  def `carousel-fade`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-indicators {
   *   position:absolute;
   *   right:0;
   *   bottom:0;
   *   left:0;
   *   z-index:2;
   *   display:flex;
   *   justify-content:center;
   *   padding:0;
   *   margin-right:15%;
   *   margin-bottom:1rem;
   *   margin-left:15%;
   * }
   * }}}
  */
  def `carousel-indicators`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-indicators-dot [data-bs-target] {
   *   width:.5rem;
   *   height:.5rem;
   *   border-radius:100rem;
   *   border:10px var(--tblr-border-style) transparent;
   *   margin:0;
   * }
   * }}}
  */
  def `carousel-indicators-dot`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-indicators-thumb [data-bs-target] {
   *   width:2rem;
   *   height:auto;
   *   background:no-repeat center/cover;
   *   border:0;
   *   border-radius:var(--tblr-border-radius);
   *   box-shadow:rgba(var(--tblr-body-color-rgb),.04) 0 2px 4px 0;
   *   margin:0 3px;
   *   opacity:.75;
   * }
   * }}}
  */
  def `carousel-indicators-thumb`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-indicators-vertical {
   *   left:auto;
   *   top:0;
   *   margin:0 1rem 0 0;
   *   flex-direction:column;
   * }
   * }}}
  */
  def `carousel-indicators-vertical`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-inner {
   *   position:relative;
   *   width:100%;
   *   overflow:hidden;
   * }
   * }}}
  */
  def `carousel-inner`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-item {
   *   position:relative;
   *   display:none;
   *   float:left;
   *   width:100%;
   *   margin-right:-100%;
   *   -webkit-backface-visibility:hidden;
   *   backface-visibility:hidden;
   *   transition:transform .6s ease-in-out;
   * }
   * }}}
  */
  def `carousel-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .active.carousel-item-end,
   * .carousel-item-next:not(.carousel-item-start) { transform:translateX(100%); }
   * }}}
  */
  def `carousel-item-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-item-next,
   * .carousel-item-prev,
   * .carousel-item.active { display:block; }
   * }}}
  */
  def `carousel-item-next`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel-item-next,
   * .carousel-item-prev,
   * .carousel-item.active { display:block; }
   * }}}
  */
  def `carousel-item-prev`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `carousel-item-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart {
   *   display:block;
   *   min-height:10rem;
   * }
   * }}}
  */
  def chart: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-lg { height:15rem; }
   * }}}
  */
  def `chart-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-sm { height:2.5rem; }
   * }}}
  */
  def `chart-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-sparkline {
   *   position:relative;
   *   width:4rem;
   *   height:2.5rem;
   *   line-height:1;
   *   min-height:0 !important;
   * }
   * }}}
  */
  def `chart-sparkline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-sparkline-label {
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   bottom:0;
   *   left:0;
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   font-size:.625rem;
   * }
   * }}}
  */
  def `chart-sparkline-label`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-sparkline-sm { height:1.5rem; }
   * }}}
  */
  def `chart-sparkline-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-sparkline-square { width:2.5rem; }
   * }}}
  */
  def `chart-sparkline-square`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-sparkline-wide { width:6rem; }
   * }}}
  */
  def `chart-sparkline-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chart-square { height:5.75rem; }
   * }}}
  */
  def `chart-square`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubble {
   *   background:var(--tblr-bg-surface-secondary);
   *   border-radius:var(--tblr-border-radius-lg);
   *   padding:1rem;
   *   position:relative;
   * }
   * }}}
  */
  def `chat-bubble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubble-author { font-weight:600; }
   * }}}
  */
  def `chat-bubble-author`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubble-body>:last-child { margin-bottom:0; }
   * }}}
  */
  def `chat-bubble-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubble-date { color:var(--tblr-secondary); }
   * }}}
  */
  def `chat-bubble-date`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubble-me {
   *   background-color:var(--tblr-primary-lt);
   *   box-shadow:none;
   * }
   * }}}
  */
  def `chat-bubble-me`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubble-title { margin-bottom:.25rem; }
   * }}}
  */
  def `chat-bubble-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .chat-bubbles {
   *   display:flex;
   *   flex-direction:column;
   *   gap:1rem;
   * }
   * }}}
  */
  def `chat-bubbles`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-clear_button .clear-button {
   *   opacity:0;
   *   position:absolute;
   *   top:50%;
   *   transform:translateY(-50%);
   *   right:calc(.75rem - 5px);
   *   margin-right:0 !important;
   *   background:0 0 !important;
   *   transition:opacity .5s;
   *   cursor:pointer;
   * }
   * }}}
  */
  def `clear-button`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .clearfix::after {
   *   display:block;
   *   clear:both;
   *   content:"";
   * }
   * }}}
  */
  def clearfix: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * input.clr-color {
   *   border-radius:var(--tblr-border-radius);
   *   color:var(--tblr-body-color);
   *   border-color:var(--tblr-border-color);
   *   background:0 0;
   * }
   * }}}
  */
  def `clr-color`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .clr-field { display:block; }
   * }}}
  */
  def `clr-field`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .clr-picker {
   *   box-shadow:var(--tblr-box-shadow-dropdown);
   *   background-color:var(--tblr-bg-surface);
   * }
   * }}}
  */
  def `clr-picker`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .clr-preview {
   *   border-radius:var(--tblr-border-radius);
   *   overflow:visible;
   * }
   * }}}
  */
  def `clr-preview`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .clr-swatches button {
   *   border-radius:var(--tblr-border-radius);
   *   padding:0 2px 4px 2px;
   * }
   * }}}
  */
  def `clr-swatches`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col { flex:1 0 0%; }
   * }}}
  */
  def col: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-1 {
   *   flex:0 0 auto;
   *   width:8.33333333%;
   * }
   * }}}
  */
  def `col-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-10 {
   *   flex:0 0 auto;
   *   width:83.33333333%;
   * }
   * }}}
  */
  def `col-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-11 {
   *   flex:0 0 auto;
   *   width:91.66666667%;
   * }
   * }}}
  */
  def `col-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-12 {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `col-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-2 {
   *   flex:0 0 auto;
   *   width:16.66666667%;
   * }
   * }}}
  */
  def `col-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-3 {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `col-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-4 {
   *   flex:0 0 auto;
   *   width:33.33333333%;
   * }
   * }}}
  */
  def `col-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-5 {
   *   flex:0 0 auto;
   *   width:41.66666667%;
   * }
   * }}}
  */
  def `col-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-6 {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `col-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-7 {
   *   flex:0 0 auto;
   *   width:58.33333333%;
   * }
   * }}}
  */
  def `col-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-8 {
   *   flex:0 0 auto;
   *   width:66.66666667%;
   * }
   * }}}
  */
  def `col-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-9 {
   *   flex:0 0 auto;
   *   width:75%;
   * }
   * }}}
  */
  def `col-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-auto {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `col-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-form-label {
   *   padding-top:calc(.5625rem + var(--tblr-border-width));
   *   padding-bottom:calc(.5625rem + var(--tblr-border-width));
   *   margin-bottom:0;
   *   font-size:inherit;
   *   font-weight:var(--tblr-font-weight-medium);
   *   line-height:1.4285714286;
   * }
   * }}}
  */
  def `col-form-label`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-form-label-lg {
   *   padding-top:calc(.5rem + var(--tblr-border-width));
   *   padding-bottom:calc(.5rem + var(--tblr-border-width));
   *   font-size:1.25rem;
   * }
   * }}}
  */
  def `col-form-label-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-form-label-sm {
   *   padding-top:calc(.125rem + var(--tblr-border-width));
   *   padding-bottom:calc(.125rem + var(--tblr-border-width));
   *   font-size:.75rem;
   * }
   * }}}
  */
  def `col-form-label-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg { flex:1 0 0%; }
   * }}}
  */
  def `col-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-1 {
   *   flex:0 0 auto;
   *   width:8.33333333%;
   * }
   * }}}
  */
  def `col-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-10 {
   *   flex:0 0 auto;
   *   width:83.33333333%;
   * }
   * }}}
  */
  def `col-lg-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-11 {
   *   flex:0 0 auto;
   *   width:91.66666667%;
   * }
   * }}}
  */
  def `col-lg-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-12 {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `col-lg-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-2 {
   *   flex:0 0 auto;
   *   width:16.66666667%;
   * }
   * }}}
  */
  def `col-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-3 {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `col-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-4 {
   *   flex:0 0 auto;
   *   width:33.33333333%;
   * }
   * }}}
  */
  def `col-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-5 {
   *   flex:0 0 auto;
   *   width:41.66666667%;
   * }
   * }}}
  */
  def `col-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-6 {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `col-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-7 {
   *   flex:0 0 auto;
   *   width:58.33333333%;
   * }
   * }}}
  */
  def `col-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-8 {
   *   flex:0 0 auto;
   *   width:66.66666667%;
   * }
   * }}}
  */
  def `col-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-9 {
   *   flex:0 0 auto;
   *   width:75%;
   * }
   * }}}
  */
  def `col-lg-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-lg-auto {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `col-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md { flex:1 0 0%; }
   * }}}
  */
  def `col-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-1 {
   *   flex:0 0 auto;
   *   width:8.33333333%;
   * }
   * }}}
  */
  def `col-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-10 {
   *   flex:0 0 auto;
   *   width:83.33333333%;
   * }
   * }}}
  */
  def `col-md-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-11 {
   *   flex:0 0 auto;
   *   width:91.66666667%;
   * }
   * }}}
  */
  def `col-md-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-12 {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `col-md-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-2 {
   *   flex:0 0 auto;
   *   width:16.66666667%;
   * }
   * }}}
  */
  def `col-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-3 {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `col-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-4 {
   *   flex:0 0 auto;
   *   width:33.33333333%;
   * }
   * }}}
  */
  def `col-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-5 {
   *   flex:0 0 auto;
   *   width:41.66666667%;
   * }
   * }}}
  */
  def `col-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-6 {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `col-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-7 {
   *   flex:0 0 auto;
   *   width:58.33333333%;
   * }
   * }}}
  */
  def `col-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-8 {
   *   flex:0 0 auto;
   *   width:66.66666667%;
   * }
   * }}}
  */
  def `col-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-9 {
   *   flex:0 0 auto;
   *   width:75%;
   * }
   * }}}
  */
  def `col-md-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-md-auto {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `col-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-separator { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color); }
   * }}}
  */
  def `col-separator`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm { flex:1 0 0%; }
   * }}}
  */
  def `col-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-1 {
   *   flex:0 0 auto;
   *   width:8.33333333%;
   * }
   * }}}
  */
  def `col-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-10 {
   *   flex:0 0 auto;
   *   width:83.33333333%;
   * }
   * }}}
  */
  def `col-sm-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-11 {
   *   flex:0 0 auto;
   *   width:91.66666667%;
   * }
   * }}}
  */
  def `col-sm-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-12 {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `col-sm-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-2 {
   *   flex:0 0 auto;
   *   width:16.66666667%;
   * }
   * }}}
  */
  def `col-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-3 {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `col-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-4 {
   *   flex:0 0 auto;
   *   width:33.33333333%;
   * }
   * }}}
  */
  def `col-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-5 {
   *   flex:0 0 auto;
   *   width:41.66666667%;
   * }
   * }}}
  */
  def `col-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-6 {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `col-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-7 {
   *   flex:0 0 auto;
   *   width:58.33333333%;
   * }
   * }}}
  */
  def `col-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-8 {
   *   flex:0 0 auto;
   *   width:66.66666667%;
   * }
   * }}}
  */
  def `col-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-9 {
   *   flex:0 0 auto;
   *   width:75%;
   * }
   * }}}
  */
  def `col-sm-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-sm-auto {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `col-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl { flex:1 0 0%; }
   * }}}
  */
  def `col-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-1 {
   *   flex:0 0 auto;
   *   width:8.33333333%;
   * }
   * }}}
  */
  def `col-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-10 {
   *   flex:0 0 auto;
   *   width:83.33333333%;
   * }
   * }}}
  */
  def `col-xl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-11 {
   *   flex:0 0 auto;
   *   width:91.66666667%;
   * }
   * }}}
  */
  def `col-xl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-12 {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `col-xl-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-2 {
   *   flex:0 0 auto;
   *   width:16.66666667%;
   * }
   * }}}
  */
  def `col-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-3 {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `col-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-4 {
   *   flex:0 0 auto;
   *   width:33.33333333%;
   * }
   * }}}
  */
  def `col-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-5 {
   *   flex:0 0 auto;
   *   width:41.66666667%;
   * }
   * }}}
  */
  def `col-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-6 {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `col-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-7 {
   *   flex:0 0 auto;
   *   width:58.33333333%;
   * }
   * }}}
  */
  def `col-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-8 {
   *   flex:0 0 auto;
   *   width:66.66666667%;
   * }
   * }}}
  */
  def `col-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-9 {
   *   flex:0 0 auto;
   *   width:75%;
   * }
   * }}}
  */
  def `col-xl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xl-auto {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `col-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl { flex:1 0 0%; }
   * }}}
  */
  def `col-xxl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-1 {
   *   flex:0 0 auto;
   *   width:8.33333333%;
   * }
   * }}}
  */
  def `col-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-10 {
   *   flex:0 0 auto;
   *   width:83.33333333%;
   * }
   * }}}
  */
  def `col-xxl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-11 {
   *   flex:0 0 auto;
   *   width:91.66666667%;
   * }
   * }}}
  */
  def `col-xxl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-12 {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `col-xxl-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-2 {
   *   flex:0 0 auto;
   *   width:16.66666667%;
   * }
   * }}}
  */
  def `col-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-3 {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `col-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-4 {
   *   flex:0 0 auto;
   *   width:33.33333333%;
   * }
   * }}}
  */
  def `col-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-5 {
   *   flex:0 0 auto;
   *   width:41.66666667%;
   * }
   * }}}
  */
  def `col-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-6 {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `col-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-7 {
   *   flex:0 0 auto;
   *   width:58.33333333%;
   * }
   * }}}
  */
  def `col-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-8 {
   *   flex:0 0 auto;
   *   width:66.66666667%;
   * }
   * }}}
  */
  def `col-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-9 {
   *   flex:0 0 auto;
   *   width:75%;
   * }
   * }}}
  */
  def `col-xxl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-xxl-auto {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `col-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .collapse:not(.show) { display:none; }
   * }}}
  */
  def collapse: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .collapsing.collapse-horizontal {
   *   width:0;
   *   height:auto;
   *   transition:width .35s ease;
   * }
   * }}}
  */
  def `collapse-horizontal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def collapsed: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .collapsing {
   *   height:0;
   *   overflow:hidden;
   *   transition:height .35s ease;
   * }
   * }}}
  */
  def collapsing: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-0 {
   *   -moz-column-gap:0 !important;
   *   column-gap:0 !important;
   * }
   * }}}
  */
  def `column-gap-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-1 {
   *   -moz-column-gap:.25rem !important;
   *   column-gap:.25rem !important;
   * }
   * }}}
  */
  def `column-gap-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-2 {
   *   -moz-column-gap:.5rem !important;
   *   column-gap:.5rem !important;
   * }
   * }}}
  */
  def `column-gap-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-3 {
   *   -moz-column-gap:1rem !important;
   *   column-gap:1rem !important;
   * }
   * }}}
  */
  def `column-gap-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-4 {
   *   -moz-column-gap:1.5rem !important;
   *   column-gap:1.5rem !important;
   * }
   * }}}
  */
  def `column-gap-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-5 {
   *   -moz-column-gap:2rem !important;
   *   column-gap:2rem !important;
   * }
   * }}}
  */
  def `column-gap-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-6 {
   *   -moz-column-gap:3rem !important;
   *   column-gap:3rem !important;
   * }
   * }}}
  */
  def `column-gap-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-7 {
   *   -moz-column-gap:5rem !important;
   *   column-gap:5rem !important;
   * }
   * }}}
  */
  def `column-gap-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-8 {
   *   -moz-column-gap:8rem !important;
   *   column-gap:8rem !important;
   * }
   * }}}
  */
  def `column-gap-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-0 {
   *   -moz-column-gap:0 !important;
   *   column-gap:0 !important;
   * }
   * }}}
  */
  def `column-gap-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-1 {
   *   -moz-column-gap:.25rem !important;
   *   column-gap:.25rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-2 {
   *   -moz-column-gap:.5rem !important;
   *   column-gap:.5rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-3 {
   *   -moz-column-gap:1rem !important;
   *   column-gap:1rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-4 {
   *   -moz-column-gap:1.5rem !important;
   *   column-gap:1.5rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-5 {
   *   -moz-column-gap:2rem !important;
   *   column-gap:2rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-6 {
   *   -moz-column-gap:3rem !important;
   *   column-gap:3rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-7 {
   *   -moz-column-gap:5rem !important;
   *   column-gap:5rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-lg-8 {
   *   -moz-column-gap:8rem !important;
   *   column-gap:8rem !important;
   * }
   * }}}
  */
  def `column-gap-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-0 {
   *   -moz-column-gap:0 !important;
   *   column-gap:0 !important;
   * }
   * }}}
  */
  def `column-gap-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-1 {
   *   -moz-column-gap:.25rem !important;
   *   column-gap:.25rem !important;
   * }
   * }}}
  */
  def `column-gap-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-2 {
   *   -moz-column-gap:.5rem !important;
   *   column-gap:.5rem !important;
   * }
   * }}}
  */
  def `column-gap-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-3 {
   *   -moz-column-gap:1rem !important;
   *   column-gap:1rem !important;
   * }
   * }}}
  */
  def `column-gap-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-4 {
   *   -moz-column-gap:1.5rem !important;
   *   column-gap:1.5rem !important;
   * }
   * }}}
  */
  def `column-gap-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-5 {
   *   -moz-column-gap:2rem !important;
   *   column-gap:2rem !important;
   * }
   * }}}
  */
  def `column-gap-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-6 {
   *   -moz-column-gap:3rem !important;
   *   column-gap:3rem !important;
   * }
   * }}}
  */
  def `column-gap-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-7 {
   *   -moz-column-gap:5rem !important;
   *   column-gap:5rem !important;
   * }
   * }}}
  */
  def `column-gap-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-md-8 {
   *   -moz-column-gap:8rem !important;
   *   column-gap:8rem !important;
   * }
   * }}}
  */
  def `column-gap-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-0 {
   *   -moz-column-gap:0 !important;
   *   column-gap:0 !important;
   * }
   * }}}
  */
  def `column-gap-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-1 {
   *   -moz-column-gap:.25rem !important;
   *   column-gap:.25rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-2 {
   *   -moz-column-gap:.5rem !important;
   *   column-gap:.5rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-3 {
   *   -moz-column-gap:1rem !important;
   *   column-gap:1rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-4 {
   *   -moz-column-gap:1.5rem !important;
   *   column-gap:1.5rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-5 {
   *   -moz-column-gap:2rem !important;
   *   column-gap:2rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-6 {
   *   -moz-column-gap:3rem !important;
   *   column-gap:3rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-7 {
   *   -moz-column-gap:5rem !important;
   *   column-gap:5rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-sm-8 {
   *   -moz-column-gap:8rem !important;
   *   column-gap:8rem !important;
   * }
   * }}}
  */
  def `column-gap-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-0 {
   *   -moz-column-gap:0 !important;
   *   column-gap:0 !important;
   * }
   * }}}
  */
  def `column-gap-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-1 {
   *   -moz-column-gap:.25rem !important;
   *   column-gap:.25rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-2 {
   *   -moz-column-gap:.5rem !important;
   *   column-gap:.5rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-3 {
   *   -moz-column-gap:1rem !important;
   *   column-gap:1rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-4 {
   *   -moz-column-gap:1.5rem !important;
   *   column-gap:1.5rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-5 {
   *   -moz-column-gap:2rem !important;
   *   column-gap:2rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-6 {
   *   -moz-column-gap:3rem !important;
   *   column-gap:3rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-7 {
   *   -moz-column-gap:5rem !important;
   *   column-gap:5rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xl-8 {
   *   -moz-column-gap:8rem !important;
   *   column-gap:8rem !important;
   * }
   * }}}
  */
  def `column-gap-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-0 {
   *   -moz-column-gap:0 !important;
   *   column-gap:0 !important;
   * }
   * }}}
  */
  def `column-gap-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-1 {
   *   -moz-column-gap:.25rem !important;
   *   column-gap:.25rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-2 {
   *   -moz-column-gap:.5rem !important;
   *   column-gap:.5rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-3 {
   *   -moz-column-gap:1rem !important;
   *   column-gap:1rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-4 {
   *   -moz-column-gap:1.5rem !important;
   *   column-gap:1.5rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-5 {
   *   -moz-column-gap:2rem !important;
   *   column-gap:2rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-6 {
   *   -moz-column-gap:3rem !important;
   *   column-gap:3rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-7 {
   *   -moz-column-gap:5rem !important;
   *   column-gap:5rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .column-gap-xxl-8 {
   *   -moz-column-gap:8rem !important;
   *   column-gap:8rem !important;
   * }
   * }}}
  */
  def `column-gap-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-2 {
   *   -moz-columns:2 !important;
   *   columns:2 !important;
   * }
   * }}}
  */
  def `columns-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-3 {
   *   -moz-columns:3 !important;
   *   columns:3 !important;
   * }
   * }}}
  */
  def `columns-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-4 {
   *   -moz-columns:4 !important;
   *   columns:4 !important;
   * }
   * }}}
  */
  def `columns-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-lg-2 {
   *   -moz-columns:2 !important;
   *   columns:2 !important;
   * }
   * }}}
  */
  def `columns-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-lg-3 {
   *   -moz-columns:3 !important;
   *   columns:3 !important;
   * }
   * }}}
  */
  def `columns-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-lg-4 {
   *   -moz-columns:4 !important;
   *   columns:4 !important;
   * }
   * }}}
  */
  def `columns-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-md-2 {
   *   -moz-columns:2 !important;
   *   columns:2 !important;
   * }
   * }}}
  */
  def `columns-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-md-3 {
   *   -moz-columns:3 !important;
   *   columns:3 !important;
   * }
   * }}}
  */
  def `columns-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-md-4 {
   *   -moz-columns:4 !important;
   *   columns:4 !important;
   * }
   * }}}
  */
  def `columns-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-sm-2 {
   *   -moz-columns:2 !important;
   *   columns:2 !important;
   * }
   * }}}
  */
  def `columns-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-sm-3 {
   *   -moz-columns:3 !important;
   *   columns:3 !important;
   * }
   * }}}
  */
  def `columns-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-sm-4 {
   *   -moz-columns:4 !important;
   *   columns:4 !important;
   * }
   * }}}
  */
  def `columns-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-xl-2 {
   *   -moz-columns:2 !important;
   *   columns:2 !important;
   * }
   * }}}
  */
  def `columns-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-xl-3 {
   *   -moz-columns:3 !important;
   *   columns:3 !important;
   * }
   * }}}
  */
  def `columns-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-xl-4 {
   *   -moz-columns:4 !important;
   *   columns:4 !important;
   * }
   * }}}
  */
  def `columns-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-xxl-2 {
   *   -moz-columns:2 !important;
   *   columns:2 !important;
   * }
   * }}}
  */
  def `columns-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-xxl-3 {
   *   -moz-columns:3 !important;
   *   columns:3 !important;
   * }
   * }}}
  */
  def `columns-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .columns-xxl-4 {
   *   -moz-columns:4 !important;
   *   columns:4 !important;
   * }
   * }}}
  */
  def `columns-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def container: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `container-fluid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `container-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `container-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container-narrow {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   *   max-width:45rem;
   * }
   * }}}
  */
  def `container-narrow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container-slim {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   *   max-width:16rem;
   * }
   * }}}
  */
  def `container-slim`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `container-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container-tight {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   *   max-width:30rem;
   * }
   * }}}
  */
  def `container-tight`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `container-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .container,
   * .container-fluid,
   * .container-lg,
   * .container-md,
   * .container-sm,
   * .container-xl,
   * .container-xxl {
   *   --tblr-gutter-x:calc(var(--tblr-page-padding)*2);
   *   --tblr-gutter-y:0;
   *   width:100%;
   *   padding-right:calc(var(--tblr-gutter-x)*.5);
   *   padding-left:calc(var(--tblr-gutter-x)*.5);
   *   margin-right:auto;
   *   margin-left:auto;
   * }
   * }}}
  */
  def `container-xxl`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__days .day-item {
   *   cursor:pointer !important;
   *   padding:.5rem 0 !important;
   *   transition:color .3s,background-color .3s,border-color .3s;
   * }
   * }}}
  */
  def container__days: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__main {
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:var(--tblr-border-radius);
   *   box-shadow:none;
   * }
   * }}}
  */
  def container__main: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__months .month-item-name,
   * .litepicker .container__months .month-item-year { font-weight:var(--tblr-font-weight-medium) !important; }
   * }}}
  */
  def container__months: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown .create,
   * .ts-dropdown .no-results,
   * .ts-dropdown .optgroup-header,
   * .ts-dropdown .option { padding:3px .75rem; }
   * }}}
  */
  def create: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-auto { cursor:auto !important; }
   * }}}
  */
  def `cursor-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-default { cursor:default !important; }
   * }}}
  */
  def `cursor-default`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-grab {
   *   cursor:-webkit-grab !important;
   *   cursor:grab !important;
   * }
   * }}}
  */
  def `cursor-grab`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-grabbing {
   *   cursor:-webkit-grabbing !important;
   *   cursor:grabbing !important;
   * }
   * }}}
  */
  def `cursor-grabbing`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-help { cursor:help !important; }
   * }}}
  */
  def `cursor-help`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-move { cursor:move !important; }
   * }}}
  */
  def `cursor-move`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-none { cursor:none !important; }
   * }}}
  */
  def `cursor-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-not-allowed { cursor:not-allowed !important; }
   * }}}
  */
  def `cursor-not-allowed`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-pointer { cursor:pointer !important; }
   * }}}
  */
  def `cursor-pointer`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-progress { cursor:progress !important; }
   * }}}
  */
  def `cursor-progress`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-text { cursor:text !important; }
   * }}}
  */
  def `cursor-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-v-text { cursor:vertical-text !important; }
   * }}}
  */
  def `cursor-v-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-wait { cursor:wait !important; }
   * }}}
  */
  def `cursor-wait`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-zoom-in { cursor:zoom-in !important; }
   * }}}
  */
  def `cursor-zoom-in`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .cursor-zoom-out { cursor:zoom-out !important; }
   * }}}
  */
  def `cursor-zoom-out`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-block { display:block !important; }
   * }}}
  */
  def `d-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-flex { display:flex !important; }
   * }}}
  */
  def `d-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-grid { display:grid !important; }
   * }}}
  */
  def `d-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-inline { display:inline !important; }
   * }}}
  */
  def `d-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-block { display:block !important; }
   * }}}
  */
  def `d-lg-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-flex { display:flex !important; }
   * }}}
  */
  def `d-lg-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-grid { display:grid !important; }
   * }}}
  */
  def `d-lg-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-inline { display:inline !important; }
   * }}}
  */
  def `d-lg-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-lg-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-lg-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-lg-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-none { display:none !important; }
   * }}}
  */
  def `d-lg-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-table { display:table !important; }
   * }}}
  */
  def `d-lg-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-lg-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-lg-table-row { display:table-row !important; }
   * }}}
  */
  def `d-lg-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-block { display:block !important; }
   * }}}
  */
  def `d-md-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-flex { display:flex !important; }
   * }}}
  */
  def `d-md-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-grid { display:grid !important; }
   * }}}
  */
  def `d-md-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-inline { display:inline !important; }
   * }}}
  */
  def `d-md-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-md-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-md-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-md-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-none { display:none !important; }
   * }}}
  */
  def `d-md-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-table { display:table !important; }
   * }}}
  */
  def `d-md-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-md-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-md-table-row { display:table-row !important; }
   * }}}
  */
  def `d-md-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-none { display:none !important; }
   * }}}
  */
  def `d-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-block { display:block !important; }
   * }}}
  */
  def `d-print-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-flex { display:flex !important; }
   * }}}
  */
  def `d-print-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-grid { display:grid !important; }
   * }}}
  */
  def `d-print-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-inline { display:inline !important; }
   * }}}
  */
  def `d-print-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-print-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-print-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-print-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-none { display:none !important; }
   * }}}
  */
  def `d-print-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-table { display:table !important; }
   * }}}
  */
  def `d-print-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-print-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-print-table-row { display:table-row !important; }
   * }}}
  */
  def `d-print-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-block { display:block !important; }
   * }}}
  */
  def `d-sm-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-flex { display:flex !important; }
   * }}}
  */
  def `d-sm-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-grid { display:grid !important; }
   * }}}
  */
  def `d-sm-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-inline { display:inline !important; }
   * }}}
  */
  def `d-sm-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-sm-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-sm-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-sm-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-none { display:none !important; }
   * }}}
  */
  def `d-sm-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-table { display:table !important; }
   * }}}
  */
  def `d-sm-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-sm-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-sm-table-row { display:table-row !important; }
   * }}}
  */
  def `d-sm-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-table { display:table !important; }
   * }}}
  */
  def `d-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-table-row { display:table-row !important; }
   * }}}
  */
  def `d-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-block { display:block !important; }
   * }}}
  */
  def `d-xl-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-flex { display:flex !important; }
   * }}}
  */
  def `d-xl-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-grid { display:grid !important; }
   * }}}
  */
  def `d-xl-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-inline { display:inline !important; }
   * }}}
  */
  def `d-xl-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-xl-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-xl-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-xl-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-none { display:none !important; }
   * }}}
  */
  def `d-xl-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-table { display:table !important; }
   * }}}
  */
  def `d-xl-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-xl-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xl-table-row { display:table-row !important; }
   * }}}
  */
  def `d-xl-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-block { display:block !important; }
   * }}}
  */
  def `d-xxl-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-flex { display:flex !important; }
   * }}}
  */
  def `d-xxl-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-grid { display:grid !important; }
   * }}}
  */
  def `d-xxl-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-inline { display:inline !important; }
   * }}}
  */
  def `d-xxl-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-inline-block { display:inline-block !important; }
   * }}}
  */
  def `d-xxl-inline-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-inline-flex { display:inline-flex !important; }
   * }}}
  */
  def `d-xxl-inline-flex`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-inline-grid { display:inline-grid !important; }
   * }}}
  */
  def `d-xxl-inline-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-none { display:none !important; }
   * }}}
  */
  def `d-xxl-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-table { display:table !important; }
   * }}}
  */
  def `d-xxl-table`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-table-cell { display:table-cell !important; }
   * }}}
  */
  def `d-xxl-table-cell`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .d-xxl-table-row { display:table-row !important; }
   * }}}
  */
  def `d-xxl-table-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .datagrid {
   *   --tblr-datagrid-padding:1.5rem;
   *   --tblr-datagrid-item-width:15rem;
   *   display:grid;
   *   grid-gap:var(--tblr-datagrid-padding);
   *   grid-template-columns:repeat(auto-fit,minmax(var(--tblr-datagrid-item-width),1fr));
   * }
   * }}}
  */
  def datagrid: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .datagrid-title {
   *   font-size:.625rem;
   *   font-weight:var(--tblr-font-weight-bold);
   *   text-transform:uppercase;
   *   letter-spacing:.04em;
   *   line-height:1rem;
   *   color:var(--tblr-secondary);
   *   margin-bottom:.25rem;
   * }
   * }}}
  */
  def `datagrid-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-date .date-item {
   *   position:relative;
   *   display:inline-block;
   *   width:1.4rem;
   *   height:1.4rem;
   *   line-height:1.4rem;
   *   color:#66758c;
   *   text-align:center;
   *   text-decoration:none;
   *   white-space:nowrap;
   *   vertical-align:middle;
   *   cursor:pointer;
   *   background:0 0;
   *   border:var(--tblr-border-width) var(--tblr-border-style) transparent;
   *   border-radius:100rem;
   *   outline:0;
   *   transition:background .3s,border .3s,box-shadow .32s,color .3s;
   * }
   * }}}
  */
  def `date-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-date .date-today {
   *   color:var(--tblr-primary);
   *   border-color:var(--tblr-border-color);
   * }
   * }}}
  */
  def `date-today`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .datepicker-inline .litepicker { box-shadow:var(--tblr-box-shadow-input); }
   * }}}
  */
  def `datepicker-inline`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__days .day-item {
   *   cursor:pointer !important;
   *   padding:.5rem 0 !important;
   *   transition:color .3s,background-color .3s,border-color .3s;
   * }
   * }}}
  */
  def `day-item`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .demo-dividers>p {
   *   opacity:.2;
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   * }
   * }}}
  */
  def `demo-dividers`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .demo-icon-preview {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   * }
   * }}}
  */
  def `demo-icon-preview`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .demo-icon-preview-icon pre {
   *   margin:0;
   *   -webkit-user-select:all;
   *   -moz-user-select:all;
   *   user-select:all;
   * }
   * }}}
  */
  def `demo-icon-preview-icon`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .demo-icons-list {
   *   display:flex;
   *   flex-wrap:wrap;
   *   padding:0;
   *   margin:0 -2px -1px 0;
   *   list-style:none;
   * }
   * }}}
  */
  def `demo-icons-list`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .demo-icons-list-item {
   *   display:flex;
   *   flex-direction:column;
   *   align-items:center;
   *   justify-content:center;
   *   aspect-ratio:1;
   *   text-align:center;
   *   padding:.5rem;
   *   border-right:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-bottom:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   color:inherit;
   *   cursor:pointer;
   * }
   * }}}
  */
  def `demo-icons-list-item`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .demo-icons-list-wrap { overflow:hidden; }
   * }}}
  */
  def `demo-icons-list-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-sort.asc,
   * .table-sort.desc,
   * .table-sort:hover { color:var(--tblr-body-color); }
   * }}}
  */
  def desc: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dimmer { position:relative; }
   * }}}
  */
  def dimmer: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dimmer.active .dimmer-content {
   *   pointer-events:none;
   *   opacity:.1;
   * }
   * }}}
  */
  def `dimmer-content`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def disabled: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .display-1 {
   *   font-size:5rem;
   *   font-weight:300;
   *   line-height:1.2;
   * }
   * }}}
  */
  def `display-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .display-2 {
   *   font-size:4.5rem;
   *   font-weight:300;
   *   line-height:1.2;
   * }
   * }}}
  */
  def `display-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .display-3 {
   *   font-size:4rem;
   *   font-weight:300;
   *   line-height:1.2;
   * }
   * }}}
  */
  def `display-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .display-4 {
   *   font-size:3.5rem;
   *   font-weight:300;
   *   line-height:1.2;
   * }
   * }}}
  */
  def `display-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .display-5 {
   *   font-size:3rem;
   *   font-weight:300;
   *   line-height:1.2;
   * }
   * }}}
  */
  def `display-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .display-6 {
   *   font-size:2rem;
   *   font-weight:300;
   *   line-height:1.2;
   * }
   * }}}
  */
  def `display-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-0>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-1>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-2>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-3>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-4>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-5>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-6>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-7>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-x-8>:not(template)~:not(template) { border-left:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-x-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-0>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-1>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-2>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-3>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-4>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-5>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-6>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-7>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-8>:not(template)~:not(template) { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent) !important; }
   * }}}
  */
  def `divide-y-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .divide-y-fill {
   *   display:flex;
   *   flex-direction:column;
   *   height:100%;
   * }
   * }}}
  */
  def `divide-y-fill`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .dl,
   * .highlight .s,
   * .highlight .s2 { color:#b5f4a5; }
   * }}}
  */
  def dl: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-control.dropdown -active { border-radius:var(--tblr-border-radius); }
   * }}}
  */
  def dropdown: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-dropdown_input.focus.dropdown-active .ts-control {
   *   box-shadow:none;
   *   border:1px solid var(--tblr-border-color);
   *   box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `dropdown-active`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown,
   * .dropdown-center,
   * .dropend,
   * .dropstart,
   * .dropup,
   * .dropup-center { position:relative; }
   * }}}
  */
  def `dropdown-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-divider {
   *   height:0;
   *   margin:var(--tblr-dropdown-divider-margin-y) 0;
   *   overflow:hidden;
   *   border-top:1px solid var(--tblr-dropdown-divider-bg);
   *   opacity:1;
   * }
   * }}}
  */
  def `dropdown-divider`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-wrapper .dropdown-header {
   *   position:relative;
   *   padding:6px .75rem;
   *   border-bottom:1px solid #d0d0d0;
   *   background:#f8f8f8;
   *   border-radius:var(--tblr-border-radius) var(--tblr-border-radius) 0 0;
   * }
   * }}}
  */
  def `dropdown-header`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper .dropdown-header-close {
   *   position:absolute;
   *   right:.75rem;
   *   top:50%;
   *   color:#182433;
   *   opacity:.4;
   *   margin-top:-12px;
   *   line-height:20px;
   *   font-size:20px !important;
   * }
   * }}}
  */
  def `dropdown-header-close`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-dropdown_input .dropdown-input {
   *   border:1px solid #d0d0d0;
   *   border-width:0 0 1px 0;
   *   display:block;
   *   padding:.5625rem .75rem;
   *   box-shadow:none;
   *   width:100%;
   *   background:0 0;
   * }
   * }}}
  */
  def `dropdown-input`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-item {
   *   display:block;
   *   width:100%;
   *   padding:var(--tblr-dropdown-item-padding-y) var(--tblr-dropdown-item-padding-x);
   *   clear:both;
   *   font-weight:400;
   *   color:var(--tblr-dropdown-link-color);
   *   text-align:inherit;
   *   white-space:nowrap;
   *   background-color:transparent;
   *   border:0;
   *   border-radius:var(--tblr-dropdown-item-border-radius,0);
   * }
   * }}}
  */
  def `dropdown-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-item-icon {
   *   width:1.25rem !important;
   *   height:1.25rem !important;
   *   margin-right:.5rem;
   *   color:var(--tblr-secondary);
   *   opacity:.7;
   *   text-align:center;
   * }
   * }}}
  */
  def `dropdown-item-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-item-indicator {
   *   margin-right:.5rem;
   *   margin-left:-.25rem;
   *   height:1.25rem;
   *   display:inline-flex;
   *   line-height:1;
   *   vertical-align:bottom;
   *   align-items:center;
   * }
   * }}}
  */
  def `dropdown-item-indicator`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-item-text {
   *   display:block;
   *   padding:var(--tblr-dropdown-item-padding-y) var(--tblr-dropdown-item-padding-x);
   *   color:var(--tblr-dropdown-link-color);
   * }
   * }}}
  */
  def `dropdown-item-text`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-control .dropdown-menu {
   *   width:100%;
   *   height:auto;
   * }
   * }}}
  */
  def `dropdown-menu`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-arrow:before {
   *   content:"";
   *   position:absolute;
   *   top:-.25rem;
   *   left:.75rem;
   *   display:block;
   *   background:inherit;
   *   width:14px;
   *   height:14px;
   *   transform:rotate(45deg);
   *   transform-origin:center;
   *   border:1px solid;
   *   border-color:inherit;
   *   z-index:-1;
   *   clip:rect(0,9px,9px,0);
   * }
   * }}}
  */
  def `dropdown-menu-arrow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-card { padding:0; }
   * }}}
  */
  def `dropdown-menu-card`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-column { min-width:11rem; }
   * }}}
  */
  def `dropdown-menu-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm .navbar-collapse .dropdown-menu-columns { flex-direction:column; }
   * }}}
  */
  def `dropdown-menu-columns`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-dark {
   *   --tblr-dropdown-color:#dadfe5;
   *   --tblr-dropdown-bg:#182433;
   *   --tblr-dropdown-border-color:var(--tblr-border-color-translucent);
   *   --tblr-dropdown-link-color:#dadfe5;
   *   --tblr-dropdown-link-hover-color:#ffffff;
   *   --tblr-dropdown-divider-bg:var(--tblr-border-color-translucent);
   *   --tblr-dropdown-link-hover-bg:rgba(255,255,255,0.15);
   *   --tblr-dropdown-link-active-color:var(--tblr-primary);
   *   --tblr-dropdown-link-active-bg:var(--tblr-active-bg);
   *   --tblr-dropdown-link-disabled-color:#929dab;
   *   --tblr-dropdown-header-color:#929dab;
   * }
   * }}}
  */
  def `dropdown-menu-dark`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .dropdown-menu-demo {
   *   display:inline-block;
   *   width:100%;
   *   position:relative;
   *   top:0;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `dropdown-menu-demo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-end { --bs-position:end; }
   * }}}
  */
  def `dropdown-menu-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-lg-end[data-bs-popper] {
   *   right:0;
   *   left:auto;
   * }
   * }}}
  */
  def `dropdown-menu-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-lg-start[data-bs-popper] {
   *   right:auto;
   *   left:0;
   * }
   * }}}
  */
  def `dropdown-menu-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-md-end[data-bs-popper] {
   *   right:0;
   *   left:auto;
   * }
   * }}}
  */
  def `dropdown-menu-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-md-start[data-bs-popper] {
   *   right:auto;
   *   left:0;
   * }
   * }}}
  */
  def `dropdown-menu-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-scrollable {
   *   height:auto;
   *   max-height:13rem;
   *   overflow-x:hidden;
   * }
   * }}}
  */
  def `dropdown-menu-scrollable`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-sm-end[data-bs-popper] {
   *   right:0;
   *   left:auto;
   * }
   * }}}
  */
  def `dropdown-menu-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-sm-start[data-bs-popper] {
   *   right:auto;
   *   left:0;
   * }
   * }}}
  */
  def `dropdown-menu-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-start { --bs-position:start; }
   * }}}
  */
  def `dropdown-menu-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-xl-end[data-bs-popper] {
   *   right:0;
   *   left:auto;
   * }
   * }}}
  */
  def `dropdown-menu-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-xl-start[data-bs-popper] {
   *   right:auto;
   *   left:0;
   * }
   * }}}
  */
  def `dropdown-menu-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-xxl-end[data-bs-popper] {
   *   right:0;
   *   left:auto;
   * }
   * }}}
  */
  def `dropdown-menu-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown-menu-xxl-start[data-bs-popper] {
   *   right:auto;
   *   left:0;
   * }
   * }}}
  */
  def `dropdown-menu-xxl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group:not(.has-validation)>.dropdown-toggle:nth-last-child(n+3),
   * .input-group:not(.has-validation)>.form-floating:not(:last-child)>.form-control,
   * .input-group:not(.has-validation)>.form-floating:not(:last-child)>.form-select,
   * .input-group:not(.has-validation)>:not(:last-child):not(.dropdown-toggle):not(.dropdown-menu):not(.form-floating) {
   *   border-top-right-radius:0;
   *   border-bottom-right-radius:0;
   * }
   * }}}
  */
  def `dropdown-toggle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-group>.btn-group:not(:last-child)>.btn,
   * .btn-group>.btn.dropdown-toggle-split:first-child,
   * .btn-group>.btn:not(:last-child):not(.dropdown-toggle) {
   *   border-top-right-radius:0;
   *   border-bottom-right-radius:0;
   * }
   * }}}
  */
  def `dropdown-toggle-split`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown,
   * .dropdown-center,
   * .dropend,
   * .dropstart,
   * .dropup,
   * .dropup-center { position:relative; }
   * }}}
  */
  def dropend: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown,
   * .dropdown-center,
   * .dropend,
   * .dropstart,
   * .dropup,
   * .dropup-center { position:relative; }
   * }}}
  */
  def dropstart: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown,
   * .dropdown-center,
   * .dropend,
   * .dropstart,
   * .dropup,
   * .dropup-center { position:relative; }
   * }}}
  */
  def dropup: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .dropdown,
   * .dropdown-center,
   * .dropend,
   * .dropstart,
   * .dropup,
   * .dropup-center { position:relative; }
   * }}}
  */
  def `dropup-center`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .dropzone {
   *   border:var(--tblr-border-width) dashed var(--tblr-border-color);
   *   color:var(--tblr-secondary);
   *   padding:1rem;
   * }
   * }}}
  */
  def dropzone: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .dropzone.dz-drag-hover {
   *   border:var(--tblr-border-width) dashed var(--tblr-primary);
   *   background:rgba(var(--tblr-primary-rgb),.01);
   *   color:var(--tblr-primary);
   * }
   * }}}
  */
  def `dz-drag-hover`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .dropzone .dz-preview .dz-image { border-radius:var(--tblr-border-radius); }
   * }}}
  */
  def `dz-image`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .dropzone.dz-drag-hover .dz-message { opacity:1; }
   * }}}
  */
  def `dz-message`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .dropzone .dz-preview { margin:.5rem; }
   * }}}
  */
  def `dz-preview`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .dropzone .dz-preview .dz-success-mark { height:54px; }
   * }}}
  */
  def `dz-success-mark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty {
   *   display:flex;
   *   flex-direction:column;
   *   align-items:center;
   *   justify-content:center;
   *   height:100%;
   *   padding:1rem;
   *   text-align:center;
   * }
   * }}}
  */
  def empty: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-action { margin-top:1.5rem; }
   * }}}
  */
  def `empty-action`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-bordered {
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def `empty-bordered`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-header {
   *   margin:0 0 1rem;
   *   font-size:4rem;
   *   font-weight:var(--tblr-font-weight-light);
   *   line-height:1;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `empty-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-icon {
   *   margin:0 0 1rem;
   *   width:3rem;
   *   height:3rem;
   *   line-height:1;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `empty-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-img {
   *   margin:0 0 2rem;
   *   line-height:1;
   * }
   * }}}
  */
  def `empty-img`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-subtitle,
   * .empty-title { margin:0 0 .5rem; }
   * }}}
  */
  def `empty-subtitle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .empty-title {
   *   font-size:1.25rem;
   *   line-height:1.75rem;
   *   font-weight:var(--tblr-font-weight-bold);
   * }
   * }}}
  */
  def `empty-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .end-0 { right:0 !important; }
   * }}}
  */
  def `end-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .end-100 { right:100% !important; }
   * }}}
  */
  def `end-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .end-50 { right:50% !important; }
   * }}}
  */
  def `end-50`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example {
   *   padding:2rem;
   *   margin:1rem 0 2rem;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:3px 3px 0 0;
   *   position:relative;
   *   min-height:12rem;
   *   display:flex;
   *   align-items:center;
   *   overflow-x:auto;
   * }
   * }}}
  */
  def example: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-bg { background:#f6f8fb; }
   * }}}
  */
  def `example-bg`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-centered { justify-content:center; }
   * }}}
  */
  def `example-centered`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-code {
   *   margin:2rem 0;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-top:none;
   * }
   * }}}
  */
  def `example-code`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-column { margin:0 auto; }
   * }}}
  */
  def `example-column`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-column-1 { max-width:26rem; }
   * }}}
  */
  def `example-column-1`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-column-2 { max-width:52rem; }
   * }}}
  */
  def `example-column-2`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-centered .example-content { flex:0 auto; }
   * }}}
  */
  def `example-content`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .example-modal-backdrop {
   *   background:#182433;
   *   opacity:.24;
   *   position:absolute;
   *   width:100%;
   *   left:0;
   *   top:0;
   *   height:100%;
   *   border-radius:2px 2px 0 0;
   * }
   * }}}
  */
  def `example-modal-backdrop`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fade { transition:opacity .15s linear; }
   * }}}
  */
  def fade: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .figure { display:inline-block; }
   * }}}
  */
  def figure: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .figure-caption {
   *   font-size:85.714285%;
   *   color:var(--tblr-secondary-color);
   * }
   * }}}
  */
  def `figure-caption`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .figure-img {
   *   margin-bottom:.5rem;
   *   line-height:1;
   * }
   * }}}
  */
  def `figure-img`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fixed-bottom {
   *   position:fixed;
   *   right:0;
   *   bottom:0;
   *   left:0;
   *   z-index:1030;
   * }
   * }}}
  */
  def `fixed-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fixed-top {
   *   position:fixed;
   *   top:0;
   *   right:0;
   *   left:0;
   *   z-index:1030;
   * }
   * }}}
  */
  def `fixed-top`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag {
   *   position:relative;
   *   display:inline-block;
   *   height:2.5rem;
   *   aspect-ratio:1.33333;
   *   background:no-repeat center/cover;
   *   box-shadow:var(--tblr-box-shadow-border);
   *   border-radius:var(--tblr-border-radius);
   *   vertical-align:bottom;
   * }
   * }}}
  */
  def flag: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-2xl { height:7rem; }
   * }}}
  */
  def `flag-2xl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ad { background-image:url(../img/flags/ad.svg); }
   * }}}
  */
  def `flag-country-ad`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ae { background-image:url(../img/flags/ae.svg); }
   * }}}
  */
  def `flag-country-ae`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-af { background-image:url(../img/flags/af.svg); }
   * }}}
  */
  def `flag-country-af`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-afrun { background-image:url(../img/flags/afrun.svg); }
   * }}}
  */
  def `flag-country-afrun`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ag { background-image:url(../img/flags/ag.svg); }
   * }}}
  */
  def `flag-country-ag`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ai { background-image:url(../img/flags/ai.svg); }
   * }}}
  */
  def `flag-country-ai`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-al { background-image:url(../img/flags/al.svg); }
   * }}}
  */
  def `flag-country-al`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-am { background-image:url(../img/flags/am.svg); }
   * }}}
  */
  def `flag-country-am`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ams { background-image:url(../img/flags/ams.svg); }
   * }}}
  */
  def `flag-country-ams`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ao { background-image:url(../img/flags/ao.svg); }
   * }}}
  */
  def `flag-country-ao`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-aq { background-image:url(../img/flags/aq.svg); }
   * }}}
  */
  def `flag-country-aq`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ar { background-image:url(../img/flags/ar.svg); }
   * }}}
  */
  def `flag-country-ar`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-as { background-image:url(../img/flags/as.svg); }
   * }}}
  */
  def `flag-country-as`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-at { background-image:url(../img/flags/at.svg); }
   * }}}
  */
  def `flag-country-at`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-au { background-image:url(../img/flags/au.svg); }
   * }}}
  */
  def `flag-country-au`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-aw { background-image:url(../img/flags/aw.svg); }
   * }}}
  */
  def `flag-country-aw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ax { background-image:url(../img/flags/ax.svg); }
   * }}}
  */
  def `flag-country-ax`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-az { background-image:url(../img/flags/az.svg); }
   * }}}
  */
  def `flag-country-az`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ba { background-image:url(../img/flags/ba.svg); }
   * }}}
  */
  def `flag-country-ba`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bb { background-image:url(../img/flags/bb.svg); }
   * }}}
  */
  def `flag-country-bb`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bd { background-image:url(../img/flags/bd.svg); }
   * }}}
  */
  def `flag-country-bd`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-be { background-image:url(../img/flags/be.svg); }
   * }}}
  */
  def `flag-country-be`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bf { background-image:url(../img/flags/bf.svg); }
   * }}}
  */
  def `flag-country-bf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bg { background-image:url(../img/flags/bg.svg); }
   * }}}
  */
  def `flag-country-bg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bh { background-image:url(../img/flags/bh.svg); }
   * }}}
  */
  def `flag-country-bh`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bi { background-image:url(../img/flags/bi.svg); }
   * }}}
  */
  def `flag-country-bi`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bj { background-image:url(../img/flags/bj.svg); }
   * }}}
  */
  def `flag-country-bj`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bl { background-image:url(../img/flags/bl.svg); }
   * }}}
  */
  def `flag-country-bl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bm { background-image:url(../img/flags/bm.svg); }
   * }}}
  */
  def `flag-country-bm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bn { background-image:url(../img/flags/bn.svg); }
   * }}}
  */
  def `flag-country-bn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bo { background-image:url(../img/flags/bo.svg); }
   * }}}
  */
  def `flag-country-bo`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bq-bo { background-image:url(../img/flags/bq-bo.svg); }
   * }}}
  */
  def `flag-country-bq-bo`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bq-sa { background-image:url(../img/flags/bq-sa.svg); }
   * }}}
  */
  def `flag-country-bq-sa`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bq-se { background-image:url(../img/flags/bq-se.svg); }
   * }}}
  */
  def `flag-country-bq-se`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-br { background-image:url(../img/flags/br.svg); }
   * }}}
  */
  def `flag-country-br`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bs { background-image:url(../img/flags/bs.svg); }
   * }}}
  */
  def `flag-country-bs`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bt { background-image:url(../img/flags/bt.svg); }
   * }}}
  */
  def `flag-country-bt`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bv { background-image:url(../img/flags/bv.svg); }
   * }}}
  */
  def `flag-country-bv`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bw { background-image:url(../img/flags/bw.svg); }
   * }}}
  */
  def `flag-country-bw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-by { background-image:url(../img/flags/by.svg); }
   * }}}
  */
  def `flag-country-by`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-bz { background-image:url(../img/flags/bz.svg); }
   * }}}
  */
  def `flag-country-bz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ca { background-image:url(../img/flags/ca.svg); }
   * }}}
  */
  def `flag-country-ca`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cc { background-image:url(../img/flags/cc.svg); }
   * }}}
  */
  def `flag-country-cc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cd { background-image:url(../img/flags/cd.svg); }
   * }}}
  */
  def `flag-country-cd`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cf { background-image:url(../img/flags/cf.svg); }
   * }}}
  */
  def `flag-country-cf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cg { background-image:url(../img/flags/cg.svg); }
   * }}}
  */
  def `flag-country-cg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ch { background-image:url(../img/flags/ch.svg); }
   * }}}
  */
  def `flag-country-ch`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ci { background-image:url(../img/flags/ci.svg); }
   * }}}
  */
  def `flag-country-ci`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ck { background-image:url(../img/flags/ck.svg); }
   * }}}
  */
  def `flag-country-ck`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cl { background-image:url(../img/flags/cl.svg); }
   * }}}
  */
  def `flag-country-cl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cm { background-image:url(../img/flags/cm.svg); }
   * }}}
  */
  def `flag-country-cm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cn { background-image:url(../img/flags/cn.svg); }
   * }}}
  */
  def `flag-country-cn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-co { background-image:url(../img/flags/co.svg); }
   * }}}
  */
  def `flag-country-co`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cr { background-image:url(../img/flags/cr.svg); }
   * }}}
  */
  def `flag-country-cr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cu { background-image:url(../img/flags/cu.svg); }
   * }}}
  */
  def `flag-country-cu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cv { background-image:url(../img/flags/cv.svg); }
   * }}}
  */
  def `flag-country-cv`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cw { background-image:url(../img/flags/cw.svg); }
   * }}}
  */
  def `flag-country-cw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cx { background-image:url(../img/flags/cx.svg); }
   * }}}
  */
  def `flag-country-cx`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cy { background-image:url(../img/flags/cy.svg); }
   * }}}
  */
  def `flag-country-cy`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-cz { background-image:url(../img/flags/cz.svg); }
   * }}}
  */
  def `flag-country-cz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-de { background-image:url(../img/flags/de.svg); }
   * }}}
  */
  def `flag-country-de`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-dj { background-image:url(../img/flags/dj.svg); }
   * }}}
  */
  def `flag-country-dj`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-dk { background-image:url(../img/flags/dk.svg); }
   * }}}
  */
  def `flag-country-dk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-dm { background-image:url(../img/flags/dm.svg); }
   * }}}
  */
  def `flag-country-dm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-do { background-image:url(../img/flags/do.svg); }
   * }}}
  */
  def `flag-country-do`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ec { background-image:url(../img/flags/ec.svg); }
   * }}}
  */
  def `flag-country-ec`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ee { background-image:url(../img/flags/ee.svg); }
   * }}}
  */
  def `flag-country-ee`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-eg { background-image:url(../img/flags/eg.svg); }
   * }}}
  */
  def `flag-country-eg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-eh { background-image:url(../img/flags/eh.svg); }
   * }}}
  */
  def `flag-country-eh`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-er { background-image:url(../img/flags/er.svg); }
   * }}}
  */
  def `flag-country-er`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-es { background-image:url(../img/flags/es.svg); }
   * }}}
  */
  def `flag-country-es`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-et { background-image:url(../img/flags/et.svg); }
   * }}}
  */
  def `flag-country-et`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-eu { background-image:url(../img/flags/eu.svg); }
   * }}}
  */
  def `flag-country-eu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-fi { background-image:url(../img/flags/fi.svg); }
   * }}}
  */
  def `flag-country-fi`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-fj { background-image:url(../img/flags/fj.svg); }
   * }}}
  */
  def `flag-country-fj`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-fk { background-image:url(../img/flags/fk.svg); }
   * }}}
  */
  def `flag-country-fk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-fm { background-image:url(../img/flags/fm.svg); }
   * }}}
  */
  def `flag-country-fm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-fo { background-image:url(../img/flags/fo.svg); }
   * }}}
  */
  def `flag-country-fo`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-fr { background-image:url(../img/flags/fr.svg); }
   * }}}
  */
  def `flag-country-fr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ga { background-image:url(../img/flags/ga.svg); }
   * }}}
  */
  def `flag-country-ga`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gb { background-image:url(../img/flags/gb.svg); }
   * }}}
  */
  def `flag-country-gb`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gb-eng { background-image:url(../img/flags/gb-eng.svg); }
   * }}}
  */
  def `flag-country-gb-eng`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gb-nir { background-image:url(../img/flags/gb-nir.svg); }
   * }}}
  */
  def `flag-country-gb-nir`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gb-sct { background-image:url(../img/flags/gb-sct.svg); }
   * }}}
  */
  def `flag-country-gb-sct`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gb-wls { background-image:url(../img/flags/gb-wls.svg); }
   * }}}
  */
  def `flag-country-gb-wls`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gd { background-image:url(../img/flags/gd.svg); }
   * }}}
  */
  def `flag-country-gd`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ge { background-image:url(../img/flags/ge.svg); }
   * }}}
  */
  def `flag-country-ge`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gf { background-image:url(../img/flags/gf.svg); }
   * }}}
  */
  def `flag-country-gf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gg { background-image:url(../img/flags/gg.svg); }
   * }}}
  */
  def `flag-country-gg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gh { background-image:url(../img/flags/gh.svg); }
   * }}}
  */
  def `flag-country-gh`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gi { background-image:url(../img/flags/gi.svg); }
   * }}}
  */
  def `flag-country-gi`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gl { background-image:url(../img/flags/gl.svg); }
   * }}}
  */
  def `flag-country-gl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gm { background-image:url(../img/flags/gm.svg); }
   * }}}
  */
  def `flag-country-gm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gn { background-image:url(../img/flags/gn.svg); }
   * }}}
  */
  def `flag-country-gn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gp { background-image:url(../img/flags/gp.svg); }
   * }}}
  */
  def `flag-country-gp`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gq { background-image:url(../img/flags/gq.svg); }
   * }}}
  */
  def `flag-country-gq`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gr { background-image:url(../img/flags/gr.svg); }
   * }}}
  */
  def `flag-country-gr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gs { background-image:url(../img/flags/gs.svg); }
   * }}}
  */
  def `flag-country-gs`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gt { background-image:url(../img/flags/gt.svg); }
   * }}}
  */
  def `flag-country-gt`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gu { background-image:url(../img/flags/gu.svg); }
   * }}}
  */
  def `flag-country-gu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gw { background-image:url(../img/flags/gw.svg); }
   * }}}
  */
  def `flag-country-gw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-gy { background-image:url(../img/flags/gy.svg); }
   * }}}
  */
  def `flag-country-gy`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-hk { background-image:url(../img/flags/hk.svg); }
   * }}}
  */
  def `flag-country-hk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-hm { background-image:url(../img/flags/hm.svg); }
   * }}}
  */
  def `flag-country-hm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-hn { background-image:url(../img/flags/hn.svg); }
   * }}}
  */
  def `flag-country-hn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-hr { background-image:url(../img/flags/hr.svg); }
   * }}}
  */
  def `flag-country-hr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ht { background-image:url(../img/flags/ht.svg); }
   * }}}
  */
  def `flag-country-ht`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-hu { background-image:url(../img/flags/hu.svg); }
   * }}}
  */
  def `flag-country-hu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-id { background-image:url(../img/flags/id.svg); }
   * }}}
  */
  def `flag-country-id`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ie { background-image:url(../img/flags/ie.svg); }
   * }}}
  */
  def `flag-country-ie`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-il { background-image:url(../img/flags/il.svg); }
   * }}}
  */
  def `flag-country-il`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-im { background-image:url(../img/flags/im.svg); }
   * }}}
  */
  def `flag-country-im`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-in { background-image:url(../img/flags/in.svg); }
   * }}}
  */
  def `flag-country-in`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-io { background-image:url(../img/flags/io.svg); }
   * }}}
  */
  def `flag-country-io`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-iq { background-image:url(../img/flags/iq.svg); }
   * }}}
  */
  def `flag-country-iq`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ir { background-image:url(../img/flags/ir.svg); }
   * }}}
  */
  def `flag-country-ir`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-is { background-image:url(../img/flags/is.svg); }
   * }}}
  */
  def `flag-country-is`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-it { background-image:url(../img/flags/it.svg); }
   * }}}
  */
  def `flag-country-it`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-je { background-image:url(../img/flags/je.svg); }
   * }}}
  */
  def `flag-country-je`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-jm { background-image:url(../img/flags/jm.svg); }
   * }}}
  */
  def `flag-country-jm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-jo { background-image:url(../img/flags/jo.svg); }
   * }}}
  */
  def `flag-country-jo`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-jp { background-image:url(../img/flags/jp.svg); }
   * }}}
  */
  def `flag-country-jp`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ke { background-image:url(../img/flags/ke.svg); }
   * }}}
  */
  def `flag-country-ke`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kg { background-image:url(../img/flags/kg.svg); }
   * }}}
  */
  def `flag-country-kg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kh { background-image:url(../img/flags/kh.svg); }
   * }}}
  */
  def `flag-country-kh`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ki { background-image:url(../img/flags/ki.svg); }
   * }}}
  */
  def `flag-country-ki`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-km { background-image:url(../img/flags/km.svg); }
   * }}}
  */
  def `flag-country-km`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kn-sk { background-image:url(../img/flags/kn-sk.svg); }
   * }}}
  */
  def `flag-country-kn-sk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kp { background-image:url(../img/flags/kp.svg); }
   * }}}
  */
  def `flag-country-kp`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kr { background-image:url(../img/flags/kr.svg); }
   * }}}
  */
  def `flag-country-kr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kw { background-image:url(../img/flags/kw.svg); }
   * }}}
  */
  def `flag-country-kw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ky { background-image:url(../img/flags/ky.svg); }
   * }}}
  */
  def `flag-country-ky`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-kz { background-image:url(../img/flags/kz.svg); }
   * }}}
  */
  def `flag-country-kz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-la { background-image:url(../img/flags/la.svg); }
   * }}}
  */
  def `flag-country-la`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lb { background-image:url(../img/flags/lb.svg); }
   * }}}
  */
  def `flag-country-lb`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lc { background-image:url(../img/flags/lc.svg); }
   * }}}
  */
  def `flag-country-lc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-li { background-image:url(../img/flags/li.svg); }
   * }}}
  */
  def `flag-country-li`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lk { background-image:url(../img/flags/lk.svg); }
   * }}}
  */
  def `flag-country-lk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lr { background-image:url(../img/flags/lr.svg); }
   * }}}
  */
  def `flag-country-lr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ls { background-image:url(../img/flags/ls.svg); }
   * }}}
  */
  def `flag-country-ls`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lt { background-image:url(../img/flags/lt.svg); }
   * }}}
  */
  def `flag-country-lt`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lu { background-image:url(../img/flags/lu.svg); }
   * }}}
  */
  def `flag-country-lu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-lv { background-image:url(../img/flags/lv.svg); }
   * }}}
  */
  def `flag-country-lv`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ly { background-image:url(../img/flags/ly.svg); }
   * }}}
  */
  def `flag-country-ly`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ma { background-image:url(../img/flags/ma.svg); }
   * }}}
  */
  def `flag-country-ma`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mc { background-image:url(../img/flags/mc.svg); }
   * }}}
  */
  def `flag-country-mc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-md { background-image:url(../img/flags/md.svg); }
   * }}}
  */
  def `flag-country-md`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-me { background-image:url(../img/flags/me.svg); }
   * }}}
  */
  def `flag-country-me`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mf { background-image:url(../img/flags/mf.svg); }
   * }}}
  */
  def `flag-country-mf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mg { background-image:url(../img/flags/mg.svg); }
   * }}}
  */
  def `flag-country-mg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mh { background-image:url(../img/flags/mh.svg); }
   * }}}
  */
  def `flag-country-mh`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mk { background-image:url(../img/flags/mk.svg); }
   * }}}
  */
  def `flag-country-mk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ml { background-image:url(../img/flags/ml.svg); }
   * }}}
  */
  def `flag-country-ml`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mm { background-image:url(../img/flags/mm.svg); }
   * }}}
  */
  def `flag-country-mm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mn { background-image:url(../img/flags/mn.svg); }
   * }}}
  */
  def `flag-country-mn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mo { background-image:url(../img/flags/mo.svg); }
   * }}}
  */
  def `flag-country-mo`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mp { background-image:url(../img/flags/mp.svg); }
   * }}}
  */
  def `flag-country-mp`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mq { background-image:url(../img/flags/mq.svg); }
   * }}}
  */
  def `flag-country-mq`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mr { background-image:url(../img/flags/mr.svg); }
   * }}}
  */
  def `flag-country-mr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ms { background-image:url(../img/flags/ms.svg); }
   * }}}
  */
  def `flag-country-ms`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mt { background-image:url(../img/flags/mt.svg); }
   * }}}
  */
  def `flag-country-mt`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mu { background-image:url(../img/flags/mu.svg); }
   * }}}
  */
  def `flag-country-mu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mv { background-image:url(../img/flags/mv.svg); }
   * }}}
  */
  def `flag-country-mv`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mw { background-image:url(../img/flags/mw.svg); }
   * }}}
  */
  def `flag-country-mw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mx { background-image:url(../img/flags/mx.svg); }
   * }}}
  */
  def `flag-country-mx`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-my { background-image:url(../img/flags/my.svg); }
   * }}}
  */
  def `flag-country-my`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-mz { background-image:url(../img/flags/mz.svg); }
   * }}}
  */
  def `flag-country-mz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-na { background-image:url(../img/flags/na.svg); }
   * }}}
  */
  def `flag-country-na`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-nc { background-image:url(../img/flags/nc.svg); }
   * }}}
  */
  def `flag-country-nc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ne { background-image:url(../img/flags/ne.svg); }
   * }}}
  */
  def `flag-country-ne`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-nf { background-image:url(../img/flags/nf.svg); }
   * }}}
  */
  def `flag-country-nf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ng { background-image:url(../img/flags/ng.svg); }
   * }}}
  */
  def `flag-country-ng`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ni { background-image:url(../img/flags/ni.svg); }
   * }}}
  */
  def `flag-country-ni`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-nl { background-image:url(../img/flags/nl.svg); }
   * }}}
  */
  def `flag-country-nl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-no { background-image:url(../img/flags/no.svg); }
   * }}}
  */
  def `flag-country-no`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag.flag-country-np {
   *   box-shadow:none;
   *   border-radius:0;
   * }
   * }}}
  */
  def `flag-country-np`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-nr { background-image:url(../img/flags/nr.svg); }
   * }}}
  */
  def `flag-country-nr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-nu { background-image:url(../img/flags/nu.svg); }
   * }}}
  */
  def `flag-country-nu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-nz { background-image:url(../img/flags/nz.svg); }
   * }}}
  */
  def `flag-country-nz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-om { background-image:url(../img/flags/om.svg); }
   * }}}
  */
  def `flag-country-om`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pa { background-image:url(../img/flags/pa.svg); }
   * }}}
  */
  def `flag-country-pa`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pe { background-image:url(../img/flags/pe.svg); }
   * }}}
  */
  def `flag-country-pe`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pf { background-image:url(../img/flags/pf.svg); }
   * }}}
  */
  def `flag-country-pf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pg { background-image:url(../img/flags/pg.svg); }
   * }}}
  */
  def `flag-country-pg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ph { background-image:url(../img/flags/ph.svg); }
   * }}}
  */
  def `flag-country-ph`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pk { background-image:url(../img/flags/pk.svg); }
   * }}}
  */
  def `flag-country-pk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pl { background-image:url(../img/flags/pl.svg); }
   * }}}
  */
  def `flag-country-pl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pm { background-image:url(../img/flags/pm.svg); }
   * }}}
  */
  def `flag-country-pm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pn { background-image:url(../img/flags/pn.svg); }
   * }}}
  */
  def `flag-country-pn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pr { background-image:url(../img/flags/pr.svg); }
   * }}}
  */
  def `flag-country-pr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ps { background-image:url(../img/flags/ps.svg); }
   * }}}
  */
  def `flag-country-ps`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pt { background-image:url(../img/flags/pt.svg); }
   * }}}
  */
  def `flag-country-pt`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-pw { background-image:url(../img/flags/pw.svg); }
   * }}}
  */
  def `flag-country-pw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-py { background-image:url(../img/flags/py.svg); }
   * }}}
  */
  def `flag-country-py`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-qa { background-image:url(../img/flags/qa.svg); }
   * }}}
  */
  def `flag-country-qa`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-rainbow { background-image:url(../img/flags/rainbow.svg); }
   * }}}
  */
  def `flag-country-rainbow`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-re { background-image:url(../img/flags/re.svg); }
   * }}}
  */
  def `flag-country-re`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ro { background-image:url(../img/flags/ro.svg); }
   * }}}
  */
  def `flag-country-ro`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-rs { background-image:url(../img/flags/rs.svg); }
   * }}}
  */
  def `flag-country-rs`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ru { background-image:url(../img/flags/ru.svg); }
   * }}}
  */
  def `flag-country-ru`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-rw { background-image:url(../img/flags/rw.svg); }
   * }}}
  */
  def `flag-country-rw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sa { background-image:url(../img/flags/sa.svg); }
   * }}}
  */
  def `flag-country-sa`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sb { background-image:url(../img/flags/sb.svg); }
   * }}}
  */
  def `flag-country-sb`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sc { background-image:url(../img/flags/sc.svg); }
   * }}}
  */
  def `flag-country-sc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sd { background-image:url(../img/flags/sd.svg); }
   * }}}
  */
  def `flag-country-sd`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-se { background-image:url(../img/flags/se.svg); }
   * }}}
  */
  def `flag-country-se`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sg { background-image:url(../img/flags/sg.svg); }
   * }}}
  */
  def `flag-country-sg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sh { background-image:url(../img/flags/sh.svg); }
   * }}}
  */
  def `flag-country-sh`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-si { background-image:url(../img/flags/si.svg); }
   * }}}
  */
  def `flag-country-si`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sj { background-image:url(../img/flags/sj.svg); }
   * }}}
  */
  def `flag-country-sj`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sk { background-image:url(../img/flags/sk.svg); }
   * }}}
  */
  def `flag-country-sk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sl { background-image:url(../img/flags/sl.svg); }
   * }}}
  */
  def `flag-country-sl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sm { background-image:url(../img/flags/sm.svg); }
   * }}}
  */
  def `flag-country-sm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sn { background-image:url(../img/flags/sn.svg); }
   * }}}
  */
  def `flag-country-sn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-so { background-image:url(../img/flags/so.svg); }
   * }}}
  */
  def `flag-country-so`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sr { background-image:url(../img/flags/sr.svg); }
   * }}}
  */
  def `flag-country-sr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ss { background-image:url(../img/flags/ss.svg); }
   * }}}
  */
  def `flag-country-ss`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-st { background-image:url(../img/flags/st.svg); }
   * }}}
  */
  def `flag-country-st`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sv { background-image:url(../img/flags/sv.svg); }
   * }}}
  */
  def `flag-country-sv`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sx { background-image:url(../img/flags/sx.svg); }
   * }}}
  */
  def `flag-country-sx`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sy { background-image:url(../img/flags/sy.svg); }
   * }}}
  */
  def `flag-country-sy`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-sz { background-image:url(../img/flags/sz.svg); }
   * }}}
  */
  def `flag-country-sz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tc { background-image:url(../img/flags/tc.svg); }
   * }}}
  */
  def `flag-country-tc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-td { background-image:url(../img/flags/td.svg); }
   * }}}
  */
  def `flag-country-td`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tf { background-image:url(../img/flags/tf.svg); }
   * }}}
  */
  def `flag-country-tf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tg { background-image:url(../img/flags/tg.svg); }
   * }}}
  */
  def `flag-country-tg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-th { background-image:url(../img/flags/th.svg); }
   * }}}
  */
  def `flag-country-th`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tj { background-image:url(../img/flags/tj.svg); }
   * }}}
  */
  def `flag-country-tj`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tk { background-image:url(../img/flags/tk.svg); }
   * }}}
  */
  def `flag-country-tk`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tl { background-image:url(../img/flags/tl.svg); }
   * }}}
  */
  def `flag-country-tl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tm { background-image:url(../img/flags/tm.svg); }
   * }}}
  */
  def `flag-country-tm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tn { background-image:url(../img/flags/tn.svg); }
   * }}}
  */
  def `flag-country-tn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-to { background-image:url(../img/flags/to.svg); }
   * }}}
  */
  def `flag-country-to`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tr { background-image:url(../img/flags/tr.svg); }
   * }}}
  */
  def `flag-country-tr`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tt { background-image:url(../img/flags/tt.svg); }
   * }}}
  */
  def `flag-country-tt`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tv { background-image:url(../img/flags/tv.svg); }
   * }}}
  */
  def `flag-country-tv`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tw { background-image:url(../img/flags/tw.svg); }
   * }}}
  */
  def `flag-country-tw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-tz { background-image:url(../img/flags/tz.svg); }
   * }}}
  */
  def `flag-country-tz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ua { background-image:url(../img/flags/ua.svg); }
   * }}}
  */
  def `flag-country-ua`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ug { background-image:url(../img/flags/ug.svg); }
   * }}}
  */
  def `flag-country-ug`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-um { background-image:url(../img/flags/um.svg); }
   * }}}
  */
  def `flag-country-um`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-unasur { background-image:url(../img/flags/unasur.svg); }
   * }}}
  */
  def `flag-country-unasur`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-us { background-image:url(../img/flags/us.svg); }
   * }}}
  */
  def `flag-country-us`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-uy { background-image:url(../img/flags/uy.svg); }
   * }}}
  */
  def `flag-country-uy`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-uz { background-image:url(../img/flags/uz.svg); }
   * }}}
  */
  def `flag-country-uz`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-va { background-image:url(../img/flags/va.svg); }
   * }}}
  */
  def `flag-country-va`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-vc { background-image:url(../img/flags/vc.svg); }
   * }}}
  */
  def `flag-country-vc`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ve { background-image:url(../img/flags/ve.svg); }
   * }}}
  */
  def `flag-country-ve`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-vg { background-image:url(../img/flags/vg.svg); }
   * }}}
  */
  def `flag-country-vg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-vi { background-image:url(../img/flags/vi.svg); }
   * }}}
  */
  def `flag-country-vi`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-vn { background-image:url(../img/flags/vn.svg); }
   * }}}
  */
  def `flag-country-vn`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-vu { background-image:url(../img/flags/vu.svg); }
   * }}}
  */
  def `flag-country-vu`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-wf { background-image:url(../img/flags/wf.svg); }
   * }}}
  */
  def `flag-country-wf`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ws { background-image:url(../img/flags/ws.svg); }
   * }}}
  */
  def `flag-country-ws`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-ye { background-image:url(../img/flags/ye.svg); }
   * }}}
  */
  def `flag-country-ye`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-za { background-image:url(../img/flags/za.svg); }
   * }}}
  */
  def `flag-country-za`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-zm { background-image:url(../img/flags/zm.svg); }
   * }}}
  */
  def `flag-country-zm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-country-zw { background-image:url(../img/flags/zw.svg); }
   * }}}
  */
  def `flag-country-zw`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-lg { height:3rem; }
   * }}}
  */
  def `flag-lg`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-md { height:2.5rem; }
   * }}}
  */
  def `flag-md`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-sm { height:2rem; }
   * }}}
  */
  def `flag-sm`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-xl { height:5rem; }
   * }}}
  */
  def `flag-xl`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-xs { height:1.25rem; }
   * }}}
  */
  def `flag-xs`: BtsClass = this

  /**
   * Files:tabler-flags.min.css;tabler-flags.rtl.min.css
   * {{{
   * .flag-xxs { height:1rem; }
   * }}}
  */
  def `flag-xxs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-column { flex-direction:column !important; }
   * }}}
  */
  def `flex-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-column-reverse { flex-direction:column-reverse !important; }
   * }}}
  */
  def `flex-column-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-fill { flex:1 1 auto !important; }
   * }}}
  */
  def `flex-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-grow-0 { flex-grow:0 !important; }
   * }}}
  */
  def `flex-grow-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-grow-1 { flex-grow:1 !important; }
   * }}}
  */
  def `flex-grow-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-column { flex-direction:column !important; }
   * }}}
  */
  def `flex-lg-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-column-reverse { flex-direction:column-reverse !important; }
   * }}}
  */
  def `flex-lg-column-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-fill { flex:1 1 auto !important; }
   * }}}
  */
  def `flex-lg-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-grow-0 { flex-grow:0 !important; }
   * }}}
  */
  def `flex-lg-grow-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-grow-1 { flex-grow:1 !important; }
   * }}}
  */
  def `flex-lg-grow-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-nowrap { flex-wrap:nowrap !important; }
   * }}}
  */
  def `flex-lg-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-row { flex-direction:row !important; }
   * }}}
  */
  def `flex-lg-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-row-reverse { flex-direction:row-reverse !important; }
   * }}}
  */
  def `flex-lg-row-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-shrink-0 { flex-shrink:0 !important; }
   * }}}
  */
  def `flex-lg-shrink-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-shrink-1 { flex-shrink:1 !important; }
   * }}}
  */
  def `flex-lg-shrink-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-wrap { flex-wrap:wrap !important; }
   * }}}
  */
  def `flex-lg-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-lg-wrap-reverse { flex-wrap:wrap-reverse !important; }
   * }}}
  */
  def `flex-lg-wrap-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-column { flex-direction:column !important; }
   * }}}
  */
  def `flex-md-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-column-reverse { flex-direction:column-reverse !important; }
   * }}}
  */
  def `flex-md-column-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-fill { flex:1 1 auto !important; }
   * }}}
  */
  def `flex-md-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-grow-0 { flex-grow:0 !important; }
   * }}}
  */
  def `flex-md-grow-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-grow-1 { flex-grow:1 !important; }
   * }}}
  */
  def `flex-md-grow-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-nowrap { flex-wrap:nowrap !important; }
   * }}}
  */
  def `flex-md-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-row { flex-direction:row !important; }
   * }}}
  */
  def `flex-md-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-row-reverse { flex-direction:row-reverse !important; }
   * }}}
  */
  def `flex-md-row-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-shrink-0 { flex-shrink:0 !important; }
   * }}}
  */
  def `flex-md-shrink-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-shrink-1 { flex-shrink:1 !important; }
   * }}}
  */
  def `flex-md-shrink-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-wrap { flex-wrap:wrap !important; }
   * }}}
  */
  def `flex-md-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-md-wrap-reverse { flex-wrap:wrap-reverse !important; }
   * }}}
  */
  def `flex-md-wrap-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-nowrap { flex-wrap:nowrap !important; }
   * }}}
  */
  def `flex-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-row { flex-direction:row !important; }
   * }}}
  */
  def `flex-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-row-reverse { flex-direction:row-reverse !important; }
   * }}}
  */
  def `flex-row-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-shrink-0 { flex-shrink:0 !important; }
   * }}}
  */
  def `flex-shrink-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-shrink-1 { flex-shrink:1 !important; }
   * }}}
  */
  def `flex-shrink-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-column { flex-direction:column !important; }
   * }}}
  */
  def `flex-sm-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-column-reverse { flex-direction:column-reverse !important; }
   * }}}
  */
  def `flex-sm-column-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-fill { flex:1 1 auto !important; }
   * }}}
  */
  def `flex-sm-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-grow-0 { flex-grow:0 !important; }
   * }}}
  */
  def `flex-sm-grow-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-grow-1 { flex-grow:1 !important; }
   * }}}
  */
  def `flex-sm-grow-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-nowrap { flex-wrap:nowrap !important; }
   * }}}
  */
  def `flex-sm-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-row { flex-direction:row !important; }
   * }}}
  */
  def `flex-sm-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-row-reverse { flex-direction:row-reverse !important; }
   * }}}
  */
  def `flex-sm-row-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-shrink-0 { flex-shrink:0 !important; }
   * }}}
  */
  def `flex-sm-shrink-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-shrink-1 { flex-shrink:1 !important; }
   * }}}
  */
  def `flex-sm-shrink-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-wrap { flex-wrap:wrap !important; }
   * }}}
  */
  def `flex-sm-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-sm-wrap-reverse { flex-wrap:wrap-reverse !important; }
   * }}}
  */
  def `flex-sm-wrap-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-wrap { flex-wrap:wrap !important; }
   * }}}
  */
  def `flex-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-wrap-reverse { flex-wrap:wrap-reverse !important; }
   * }}}
  */
  def `flex-wrap-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-column { flex-direction:column !important; }
   * }}}
  */
  def `flex-xl-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-column-reverse { flex-direction:column-reverse !important; }
   * }}}
  */
  def `flex-xl-column-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-fill { flex:1 1 auto !important; }
   * }}}
  */
  def `flex-xl-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-grow-0 { flex-grow:0 !important; }
   * }}}
  */
  def `flex-xl-grow-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-grow-1 { flex-grow:1 !important; }
   * }}}
  */
  def `flex-xl-grow-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-nowrap { flex-wrap:nowrap !important; }
   * }}}
  */
  def `flex-xl-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-row { flex-direction:row !important; }
   * }}}
  */
  def `flex-xl-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-row-reverse { flex-direction:row-reverse !important; }
   * }}}
  */
  def `flex-xl-row-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-shrink-0 { flex-shrink:0 !important; }
   * }}}
  */
  def `flex-xl-shrink-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-shrink-1 { flex-shrink:1 !important; }
   * }}}
  */
  def `flex-xl-shrink-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-wrap { flex-wrap:wrap !important; }
   * }}}
  */
  def `flex-xl-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xl-wrap-reverse { flex-wrap:wrap-reverse !important; }
   * }}}
  */
  def `flex-xl-wrap-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-column { flex-direction:column !important; }
   * }}}
  */
  def `flex-xxl-column`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-column-reverse { flex-direction:column-reverse !important; }
   * }}}
  */
  def `flex-xxl-column-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-fill { flex:1 1 auto !important; }
   * }}}
  */
  def `flex-xxl-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-grow-0 { flex-grow:0 !important; }
   * }}}
  */
  def `flex-xxl-grow-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-grow-1 { flex-grow:1 !important; }
   * }}}
  */
  def `flex-xxl-grow-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-nowrap { flex-wrap:nowrap !important; }
   * }}}
  */
  def `flex-xxl-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-row { flex-direction:row !important; }
   * }}}
  */
  def `flex-xxl-row`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-row-reverse { flex-direction:row-reverse !important; }
   * }}}
  */
  def `flex-xxl-row-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-shrink-0 { flex-shrink:0 !important; }
   * }}}
  */
  def `flex-xxl-shrink-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-shrink-1 { flex-shrink:1 !important; }
   * }}}
  */
  def `flex-xxl-shrink-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-wrap { flex-wrap:wrap !important; }
   * }}}
  */
  def `flex-xxl-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .flex-xxl-wrap-reverse { flex-wrap:wrap-reverse !important; }
   * }}}
  */
  def `flex-xxl-wrap-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-end { float:right !important; }
   * }}}
  */
  def `float-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-lg-end { float:right !important; }
   * }}}
  */
  def `float-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-lg-none { float:none !important; }
   * }}}
  */
  def `float-lg-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-lg-start { float:left !important; }
   * }}}
  */
  def `float-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-md-end { float:right !important; }
   * }}}
  */
  def `float-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-md-none { float:none !important; }
   * }}}
  */
  def `float-md-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-md-start { float:left !important; }
   * }}}
  */
  def `float-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-none { float:none !important; }
   * }}}
  */
  def `float-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-sm-end { float:right !important; }
   * }}}
  */
  def `float-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-sm-none { float:none !important; }
   * }}}
  */
  def `float-sm-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-sm-start { float:left !important; }
   * }}}
  */
  def `float-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-start { float:left !important; }
   * }}}
  */
  def `float-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-xl-end { float:right !important; }
   * }}}
  */
  def `float-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-xl-none { float:none !important; }
   * }}}
  */
  def `float-xl-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-xl-start { float:left !important; }
   * }}}
  */
  def `float-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-xxl-end { float:right !important; }
   * }}}
  */
  def `float-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-xxl-none { float:none !important; }
   * }}}
  */
  def `float-xxl-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .float-xxl-start { float:left !important; }
   * }}}
  */
  def `float-xxl-start`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-clear_button.focus.has-items .clear-button,
   * .plugin-clear_button:not(.disabled):hover.has-items .clear-button { opacity:1; }
   * }}}
  */
  def focus: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring:focus {
   *   outline:0;
   *   box-shadow:var(--tblr-focus-ring-x,0) var(--tblr-focus-ring-y,0) var(--tblr-focus-ring-blur,0) var(--tblr-focus-ring-width) var(--tblr-focus-ring-color);
   * }
   * }}}
  */
  def `focus-ring`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-azure { --tblr-focus-ring-color:rgba(var(--tblr-azure-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-bitbucket { --tblr-focus-ring-color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-blue { --tblr-focus-ring-color:rgba(var(--tblr-blue-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-cyan { --tblr-focus-ring-color:rgba(var(--tblr-cyan-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-danger { --tblr-focus-ring-color:rgba(var(--tblr-danger-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-dark { --tblr-focus-ring-color:rgba(var(--tblr-dark-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-dribbble { --tblr-focus-ring-color:rgba(var(--tblr-dribbble-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-facebook { --tblr-focus-ring-color:rgba(var(--tblr-facebook-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-flickr { --tblr-focus-ring-color:rgba(var(--tblr-flickr-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-github { --tblr-focus-ring-color:rgba(var(--tblr-github-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-google { --tblr-focus-ring-color:rgba(var(--tblr-google-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-green { --tblr-focus-ring-color:rgba(var(--tblr-green-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-indigo { --tblr-focus-ring-color:rgba(var(--tblr-indigo-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-info { --tblr-focus-ring-color:rgba(var(--tblr-info-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-instagram { --tblr-focus-ring-color:rgba(var(--tblr-instagram-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-light { --tblr-focus-ring-color:rgba(var(--tblr-light-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-lime { --tblr-focus-ring-color:rgba(var(--tblr-lime-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-linkedin { --tblr-focus-ring-color:rgba(var(--tblr-linkedin-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-muted { --tblr-focus-ring-color:rgba(var(--tblr-muted-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-orange { --tblr-focus-ring-color:rgba(var(--tblr-orange-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-pink { --tblr-focus-ring-color:rgba(var(--tblr-pink-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-pinterest { --tblr-focus-ring-color:rgba(var(--tblr-pinterest-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-primary { --tblr-focus-ring-color:rgba(var(--tblr-primary-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-purple { --tblr-focus-ring-color:rgba(var(--tblr-purple-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-red { --tblr-focus-ring-color:rgba(var(--tblr-red-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-rss { --tblr-focus-ring-color:rgba(var(--tblr-rss-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-secondary { --tblr-focus-ring-color:rgba(var(--tblr-secondary-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-success { --tblr-focus-ring-color:rgba(var(--tblr-success-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-tabler { --tblr-focus-ring-color:rgba(var(--tblr-tabler-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-teal { --tblr-focus-ring-color:rgba(var(--tblr-teal-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-twitter { --tblr-focus-ring-color:rgba(var(--tblr-twitter-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-vimeo { --tblr-focus-ring-color:rgba(var(--tblr-vimeo-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-vk { --tblr-focus-ring-color:rgba(var(--tblr-vk-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-warning { --tblr-focus-ring-color:rgba(var(--tblr-warning-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-yellow { --tblr-focus-ring-color:rgba(var(--tblr-yellow-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .focus-ring-youtube { --tblr-focus-ring-color:rgba(var(--tblr-youtube-rgb),var(--tblr-focus-ring-opacity)); }
   * }}}
  */
  def `focus-ring-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .font-monospace { font-family:var(--tblr-font-monospace) !important; }
   * }}}
  */
  def `font-monospace`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .footer {
   *   border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   background-color:#fff;
   *   padding:2rem 0;
   *   color:var(--tblr-secondary);
   *   margin-top:auto;
   * }
   * }}}
  */
  def footer: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .footer-transparent {
   *   background-color:transparent;
   *   border-top:0;
   * }
   * }}}
  */
  def `footer-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check {
   *   display:block;
   *   min-height:1.25rem;
   *   padding-left:2rem;
   *   margin-bottom:.75rem;
   * }
   * }}}
  */
  def `form-check`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check-description {
   *   display:block;
   *   color:var(--tblr-secondary);
   *   font-size:.75rem;
   *   margin-top:.25rem;
   * }
   * }}}
  */
  def `form-check-description`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check.form-check-highlight .form-check-input:not(:checked)~.form-check-label { color:var(--tblr-secondary); }
   * }}}
  */
  def `form-check-highlight`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check-inline {
   *   display:inline-block;
   *   margin-right:1rem;
   * }
   * }}}
  */
  def `form-check-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check .form-check-input {
   *   float:left;
   *   margin-left:-2rem;
   * }
   * }}}
  */
  def `form-check-input`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check-input:disabled~.form-check-label,
   * .form-check-input[disabled]~.form-check-label {
   *   cursor:default;
   *   opacity:.7;
   * }
   * }}}
  */
  def `form-check-label`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check .form-check-label-off { color:var(--tblr-secondary); }
   * }}}
  */
  def `form-check-label-off`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check .form-check-input:not(:checked)~.form-check-label-on { display:none; }
   * }}}
  */
  def `form-check-label-on`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check-reverse {
   *   padding-right:2rem;
   *   padding-left:0;
   *   text-align:right;
   * }
   * }}}
  */
  def `form-check-reverse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-check-single { margin:0; }
   * }}}
  */
  def `form-check-single`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-colorinput {
   *   position:relative;
   *   display:inline-block;
   *   margin:0;
   *   line-height:1;
   *   cursor:pointer;
   * }
   * }}}
  */
  def `form-colorinput`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-colorinput-color {
   *   display:block;
   *   width:1.5rem;
   *   height:1.5rem;
   *   color:#fff;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent);
   *   border-radius:3px;
   *   box-shadow:0 1px 2px 0 rgba(0,0,0,.05);
   * }
   * }}}
  */
  def `form-colorinput-color`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-colorinput-input {
   *   position:absolute;
   *   z-index:-1;
   *   opacity:0;
   * }
   * }}}
  */
  def `form-colorinput-input`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-colorinput-light .form-colorinput-color:before { background-image:url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 16 16' width='16' height='16'%3e%3cpath fill='none' stroke='%23182433' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' d='M4 8.5l2.5 2.5l5.5 -5.5'/%3e%3c/svg%3e"); }
   * }}}
  */
  def `form-colorinput-light`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-wrapper.form-control,
   * .ts-wrapper.form-select {
   *   padding:0 !important;
   *   height:auto;
   *   box-shadow:none;
   *   display:flex;
   * }
   * }}}
  */
  def `form-control`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control-color {
   *   width:3rem;
   *   height:calc(1.4285714286em + 1.125rem + calc(var(--tblr-border-width)*2));
   *   padding:.5625rem;
   * }
   * }}}
  */
  def `form-control-color`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control-dark {
   *   background-color:rgba(0,0,0,.1);
   *   color:#fff;
   *   border-color:transparent;
   * }
   * }}}
  */
  def `form-control-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control-flush {
   *   padding:0;
   *   background:0 0 !important;
   *   border-color:transparent !important;
   *   resize:none;
   *   box-shadow:none !important;
   *   line-height:inherit;
   * }
   * }}}
  */
  def `form-control-flush`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-lg>.ts-wrapper,
   * .ts-wrapper.form-control-lg,
   * .ts-wrapper.form-select-lg { min-height:calc(1.4285714286em + 1rem + calc(var(--tblr-border-width)*2)); }
   * }}}
  */
  def `form-control-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control-light {
   *   background-color:var(--tblr-gray-100);
   *   border-color:transparent;
   * }
   * }}}
  */
  def `form-control-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control-plaintext {
   *   display:block;
   *   width:100%;
   *   padding:.5625rem 0;
   *   margin-bottom:0;
   *   line-height:1.4285714286;
   *   color:var(--tblr-body-color);
   *   background-color:transparent;
   *   border:solid transparent;
   *   border-width:var(--tblr-border-width) 0;
   * }
   * }}}
  */
  def `form-control-plaintext`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control-rounded { border-radius:10rem; }
   * }}}
  */
  def `form-control-rounded`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-sm>.ts-wrapper,
   * .ts-wrapper.form-control-sm,
   * .ts-wrapper.form-select-sm { min-height:calc(1.4285714286em + .25rem + calc(var(--tblr-border-width)*2)); }
   * }}}
  */
  def `form-control-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-fieldset {
   *   padding:1rem;
   *   margin-bottom:1rem;
   *   background:var(--tblr-body-bg);
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def `form-fieldset`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-file-button {
   *   margin-left:0;
   *   border-left:0;
   * }
   * }}}
  */
  def `form-file-button`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-floating { position:relative; }
   * }}}
  */
  def `form-floating`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-footer { margin-top:2rem; }
   * }}}
  */
  def `form-footer`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-help {
   *   display:inline-flex;
   *   font-weight:var(--tblr-font-weight-bold);
   *   align-items:center;
   *   justify-content:center;
   *   width:1.125rem;
   *   height:1.125rem;
   *   font-size:.75rem;
   *   color:var(--tblr-secondary);
   *   text-align:center;
   *   text-decoration:none;
   *   cursor:pointer;
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   *   background:var(--tblr-gray-100);
   *   border-radius:100rem;
   *   transition:background-color .3s,color .3s;
   * }
   * }}}
  */
  def `form-help`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-hint {
   *   display:block;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `form-hint`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-imagecheck {
   *   position:relative;
   *   margin:0;
   *   cursor:pointer;
   * }
   * }}}
  */
  def `form-imagecheck`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-imagecheck-caption {
   *   padding:.25rem;
   *   font-size:.765625rem;
   *   color:var(--tblr-secondary);
   *   text-align:center;
   *   transition:color .3s;
   * }
   * }}}
  */
  def `form-imagecheck-caption`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-imagecheck-figure {
   *   position:relative;
   *   display:block;
   *   margin:0;
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:3px;
   * }
   * }}}
  */
  def `form-imagecheck-figure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-imagecheck-image {
   *   max-width:100%;
   *   display:block;
   *   opacity:.64;
   *   transition:opacity .3s;
   * }
   * }}}
  */
  def `form-imagecheck-image`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-imagecheck-input {
   *   position:absolute;
   *   z-index:-1;
   *   opacity:0;
   * }
   * }}}
  */
  def `form-imagecheck-input`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-label {
   *   margin-bottom:.5rem;
   *   font-size:.875rem;
   *   font-weight:var(--tblr-font-weight-medium);
   * }
   * }}}
  */
  def `form-label`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-label-description {
   *   float:right;
   *   font-weight:var(--tblr-font-weight-normal);
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `form-label-description`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-range {
   *   width:100%;
   *   height:1.25rem;
   *   padding:0;
   *   -webkit-appearance:none;
   *   -moz-appearance:none;
   *   appearance:none;
   *   background-color:transparent;
   * }
   * }}}
  */
  def `form-range`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .plugin-clear_button.form-select .clear-button,
   * .plugin-clear_button.single .clear-button { right:max(var(--ts-pr-caret),.75rem); }
   * }}}
  */
  def `form-select`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-lg>.ts-wrapper,
   * .ts-wrapper.form-control-lg,
   * .ts-wrapper.form-select-lg { min-height:calc(1.4285714286em + 1rem + calc(var(--tblr-border-width)*2)); }
   * }}}
  */
  def `form-select-lg`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-sm>.ts-wrapper,
   * .ts-wrapper.form-control-sm,
   * .ts-wrapper.form-select-sm { min-height:calc(1.4285714286em + .25rem + calc(var(--tblr-border-width)*2)); }
   * }}}
  */
  def `form-select-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup {
   *   display:inline-flex;
   *   margin:0 -.5rem -.5rem 0;
   *   flex-wrap:wrap;
   * }
   * }}}
  */
  def `form-selectgroup`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-boxes .form-selectgroup-label {
   *   text-align:left;
   *   padding:1.25rem 1.25rem;
   *   color:inherit;
   * }
   * }}}
  */
  def `form-selectgroup-boxes`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-check {
   *   display:inline-block;
   *   width:1.25rem;
   *   height:1.25rem;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent);
   *   vertical-align:middle;
   *   box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `form-selectgroup-check`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-check-floated {
   *   position:absolute;
   *   top:.5625rem;
   *   right:.5625rem;
   * }
   * }}}
  */
  def `form-selectgroup-check-floated`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-input {
   *   position:absolute;
   *   top:0;
   *   left:0;
   *   z-index:-1;
   *   opacity:0;
   * }
   * }}}
  */
  def `form-selectgroup-input`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup .form-selectgroup-item { margin:0 .5rem .5rem 0; }
   * }}}
  */
  def `form-selectgroup-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-label {
   *   position:relative;
   *   display:block;
   *   min-width:calc(1.4285714286em + 1.125rem + calc(var(--tblr-border-width)*2));
   *   margin:0;
   *   padding:.5625rem .75rem;
   *   font-size:.875rem;
   *   line-height:1.4285714286;
   *   color:var(--tblr-secondary);
   *   background:var(--tblr-bg-forms);
   *   text-align:center;
   *   cursor:pointer;
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   border-radius:3px;
   *   box-shadow:var(--tblr-box-shadow-input);
   *   transition:border-color .3s,background .3s,color .3s;
   * }
   * }}}
  */
  def `form-selectgroup-label`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-boxes .form-selectgroup-input:checked+.form-selectgroup-label .form-selectgroup-label-content { opacity:1; }
   * }}}
  */
  def `form-selectgroup-label-content`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-pills {
   *   flex-wrap:wrap;
   *   align-items:flex-start;
   * }
   * }}}
  */
  def `form-selectgroup-pills`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-boxes .form-selectgroup-input:checked+.form-selectgroup-label .form-selectgroup-title { color:var(--tblr-primary); }
   * }}}
  */
  def `form-selectgroup-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-selectgroup-vertical { flex-direction:column; }
   * }}}
  */
  def `form-selectgroup-vertical`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-switch { padding-left:2.5rem; }
   * }}}
  */
  def `form-switch`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-switch-lg {
   *   padding-left:3.5rem;
   *   min-height:1.5rem;
   * }
   * }}}
  */
  def `form-switch-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-text {
   *   margin-top:.25rem;
   *   font-size:85.714285%;
   *   color:var(--tblr-secondary-color);
   * }
   * }}}
  */
  def `form-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fs-1 { font-size:1.5rem !important; }
   * }}}
  */
  def `fs-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fs-2 { font-size:1.25rem !important; }
   * }}}
  */
  def `fs-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fs-3 { font-size:1rem !important; }
   * }}}
  */
  def `fs-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fs-4 { font-size:.875rem !important; }
   * }}}
  */
  def `fs-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fs-5 { font-size:.75rem !important; }
   * }}}
  */
  def `fs-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fs-6 { font-size:.625rem !important; }
   * }}}
  */
  def `fs-6`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .fslightbox-container {
   *   font-family:inherit !important;
   *   background:rgba(24,36,51,.24) !important;
   *   -webkit-backdrop-filter:blur(4px) !important;
   *   backdrop-filter:blur(4px) !important;
   * }
   * }}}
  */
  def `fslightbox-container`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .fslightbox-slash { background:currentColor !important; }
   * }}}
  */
  def `fslightbox-slash`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .fslightbox-slide-number-container { color:inherit !important; }
   * }}}
  */
  def `fslightbox-slide-number-container`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fst-italic { font-style:italic !important; }
   * }}}
  */
  def `fst-italic`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fst-normal { font-style:normal !important; }
   * }}}
  */
  def `fst-normal`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .full .ts-control { background-color:var(--tblr-bg-forms); }
   * }}}
  */
  def full: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-bold { font-weight:600 !important; }
   * }}}
  */
  def `fw-bold`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-bolder { font-weight:bolder !important; }
   * }}}
  */
  def `fw-bolder`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-light { font-weight:300 !important; }
   * }}}
  */
  def `fw-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-lighter { font-weight:lighter !important; }
   * }}}
  */
  def `fw-lighter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-medium { font-weight:500 !important; }
   * }}}
  */
  def `fw-medium`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-normal { font-weight:400 !important; }
   * }}}
  */
  def `fw-normal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .fw-semibold { font-weight:600 !important; }
   * }}}
  */
  def `fw-semibold`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-0,
   * .gx-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `g-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-1,
   * .gx-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `g-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-2,
   * .gx-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `g-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-3,
   * .gx-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `g-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-4,
   * .gx-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `g-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-5,
   * .gx-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `g-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-6,
   * .gx-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `g-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-7,
   * .gx-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `g-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-8,
   * .gx-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `g-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-1 { grid-column:auto/span 1; }
   * }}}
  */
  def `g-col-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-10 { grid-column:auto/span 10; }
   * }}}
  */
  def `g-col-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-11 { grid-column:auto/span 11; }
   * }}}
  */
  def `g-col-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-12 { grid-column:auto/span 12; }
   * }}}
  */
  def `g-col-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-2 { grid-column:auto/span 2; }
   * }}}
  */
  def `g-col-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-3 { grid-column:auto/span 3; }
   * }}}
  */
  def `g-col-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-4 { grid-column:auto/span 4; }
   * }}}
  */
  def `g-col-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-5 { grid-column:auto/span 5; }
   * }}}
  */
  def `g-col-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-6 { grid-column:auto/span 6; }
   * }}}
  */
  def `g-col-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-7 { grid-column:auto/span 7; }
   * }}}
  */
  def `g-col-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-8 { grid-column:auto/span 8; }
   * }}}
  */
  def `g-col-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-9 { grid-column:auto/span 9; }
   * }}}
  */
  def `g-col-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-1 { grid-column:auto/span 1; }
   * }}}
  */
  def `g-col-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-10 { grid-column:auto/span 10; }
   * }}}
  */
  def `g-col-lg-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-11 { grid-column:auto/span 11; }
   * }}}
  */
  def `g-col-lg-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-12 { grid-column:auto/span 12; }
   * }}}
  */
  def `g-col-lg-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-2 { grid-column:auto/span 2; }
   * }}}
  */
  def `g-col-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-3 { grid-column:auto/span 3; }
   * }}}
  */
  def `g-col-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-4 { grid-column:auto/span 4; }
   * }}}
  */
  def `g-col-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-5 { grid-column:auto/span 5; }
   * }}}
  */
  def `g-col-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-6 { grid-column:auto/span 6; }
   * }}}
  */
  def `g-col-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-7 { grid-column:auto/span 7; }
   * }}}
  */
  def `g-col-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-8 { grid-column:auto/span 8; }
   * }}}
  */
  def `g-col-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-lg-9 { grid-column:auto/span 9; }
   * }}}
  */
  def `g-col-lg-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-1 { grid-column:auto/span 1; }
   * }}}
  */
  def `g-col-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-10 { grid-column:auto/span 10; }
   * }}}
  */
  def `g-col-md-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-11 { grid-column:auto/span 11; }
   * }}}
  */
  def `g-col-md-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-12 { grid-column:auto/span 12; }
   * }}}
  */
  def `g-col-md-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-2 { grid-column:auto/span 2; }
   * }}}
  */
  def `g-col-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-3 { grid-column:auto/span 3; }
   * }}}
  */
  def `g-col-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-4 { grid-column:auto/span 4; }
   * }}}
  */
  def `g-col-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-5 { grid-column:auto/span 5; }
   * }}}
  */
  def `g-col-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-6 { grid-column:auto/span 6; }
   * }}}
  */
  def `g-col-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-7 { grid-column:auto/span 7; }
   * }}}
  */
  def `g-col-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-8 { grid-column:auto/span 8; }
   * }}}
  */
  def `g-col-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-md-9 { grid-column:auto/span 9; }
   * }}}
  */
  def `g-col-md-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-1 { grid-column:auto/span 1; }
   * }}}
  */
  def `g-col-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-10 { grid-column:auto/span 10; }
   * }}}
  */
  def `g-col-sm-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-11 { grid-column:auto/span 11; }
   * }}}
  */
  def `g-col-sm-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-12 { grid-column:auto/span 12; }
   * }}}
  */
  def `g-col-sm-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-2 { grid-column:auto/span 2; }
   * }}}
  */
  def `g-col-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-3 { grid-column:auto/span 3; }
   * }}}
  */
  def `g-col-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-4 { grid-column:auto/span 4; }
   * }}}
  */
  def `g-col-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-5 { grid-column:auto/span 5; }
   * }}}
  */
  def `g-col-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-6 { grid-column:auto/span 6; }
   * }}}
  */
  def `g-col-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-7 { grid-column:auto/span 7; }
   * }}}
  */
  def `g-col-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-8 { grid-column:auto/span 8; }
   * }}}
  */
  def `g-col-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-sm-9 { grid-column:auto/span 9; }
   * }}}
  */
  def `g-col-sm-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-1 { grid-column:auto/span 1; }
   * }}}
  */
  def `g-col-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-10 { grid-column:auto/span 10; }
   * }}}
  */
  def `g-col-xl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-11 { grid-column:auto/span 11; }
   * }}}
  */
  def `g-col-xl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-12 { grid-column:auto/span 12; }
   * }}}
  */
  def `g-col-xl-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-2 { grid-column:auto/span 2; }
   * }}}
  */
  def `g-col-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-3 { grid-column:auto/span 3; }
   * }}}
  */
  def `g-col-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-4 { grid-column:auto/span 4; }
   * }}}
  */
  def `g-col-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-5 { grid-column:auto/span 5; }
   * }}}
  */
  def `g-col-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-6 { grid-column:auto/span 6; }
   * }}}
  */
  def `g-col-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-7 { grid-column:auto/span 7; }
   * }}}
  */
  def `g-col-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-8 { grid-column:auto/span 8; }
   * }}}
  */
  def `g-col-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xl-9 { grid-column:auto/span 9; }
   * }}}
  */
  def `g-col-xl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-1 { grid-column:auto/span 1; }
   * }}}
  */
  def `g-col-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-10 { grid-column:auto/span 10; }
   * }}}
  */
  def `g-col-xxl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-11 { grid-column:auto/span 11; }
   * }}}
  */
  def `g-col-xxl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-12 { grid-column:auto/span 12; }
   * }}}
  */
  def `g-col-xxl-12`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-2 { grid-column:auto/span 2; }
   * }}}
  */
  def `g-col-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-3 { grid-column:auto/span 3; }
   * }}}
  */
  def `g-col-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-4 { grid-column:auto/span 4; }
   * }}}
  */
  def `g-col-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-5 { grid-column:auto/span 5; }
   * }}}
  */
  def `g-col-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-6 { grid-column:auto/span 6; }
   * }}}
  */
  def `g-col-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-7 { grid-column:auto/span 7; }
   * }}}
  */
  def `g-col-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-8 { grid-column:auto/span 8; }
   * }}}
  */
  def `g-col-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-col-xxl-9 { grid-column:auto/span 9; }
   * }}}
  */
  def `g-col-xxl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-0,
   * .gy-lg-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `g-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-1,
   * .gy-lg-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `g-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-2,
   * .gy-lg-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `g-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-3,
   * .gy-lg-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `g-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-4,
   * .gy-lg-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `g-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-5,
   * .gy-lg-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `g-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-6,
   * .gy-lg-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `g-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-7,
   * .gy-lg-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `g-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-8,
   * .gy-lg-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `g-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-0,
   * .gy-md-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `g-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-1,
   * .gy-md-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `g-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-2,
   * .gy-md-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `g-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-3,
   * .gy-md-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `g-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-4,
   * .gy-md-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `g-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-5,
   * .gy-md-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `g-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-6,
   * .gy-md-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `g-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-7,
   * .gy-md-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `g-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-8,
   * .gy-md-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `g-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-0,
   * .gy-sm-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `g-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-1,
   * .gy-sm-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `g-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-2,
   * .gy-sm-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `g-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-3,
   * .gy-sm-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `g-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-4,
   * .gy-sm-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `g-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-5,
   * .gy-sm-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `g-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-6,
   * .gy-sm-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `g-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-7,
   * .gy-sm-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `g-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-8,
   * .gy-sm-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `g-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-1 { grid-column-start:1; }
   * }}}
  */
  def `g-start-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-10 { grid-column-start:10; }
   * }}}
  */
  def `g-start-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-11 { grid-column-start:11; }
   * }}}
  */
  def `g-start-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-2 { grid-column-start:2; }
   * }}}
  */
  def `g-start-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-3 { grid-column-start:3; }
   * }}}
  */
  def `g-start-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-4 { grid-column-start:4; }
   * }}}
  */
  def `g-start-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-5 { grid-column-start:5; }
   * }}}
  */
  def `g-start-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-6 { grid-column-start:6; }
   * }}}
  */
  def `g-start-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-7 { grid-column-start:7; }
   * }}}
  */
  def `g-start-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-8 { grid-column-start:8; }
   * }}}
  */
  def `g-start-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-9 { grid-column-start:9; }
   * }}}
  */
  def `g-start-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-1 { grid-column-start:1; }
   * }}}
  */
  def `g-start-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-10 { grid-column-start:10; }
   * }}}
  */
  def `g-start-lg-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-11 { grid-column-start:11; }
   * }}}
  */
  def `g-start-lg-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-2 { grid-column-start:2; }
   * }}}
  */
  def `g-start-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-3 { grid-column-start:3; }
   * }}}
  */
  def `g-start-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-4 { grid-column-start:4; }
   * }}}
  */
  def `g-start-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-5 { grid-column-start:5; }
   * }}}
  */
  def `g-start-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-6 { grid-column-start:6; }
   * }}}
  */
  def `g-start-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-7 { grid-column-start:7; }
   * }}}
  */
  def `g-start-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-8 { grid-column-start:8; }
   * }}}
  */
  def `g-start-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-lg-9 { grid-column-start:9; }
   * }}}
  */
  def `g-start-lg-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-1 { grid-column-start:1; }
   * }}}
  */
  def `g-start-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-10 { grid-column-start:10; }
   * }}}
  */
  def `g-start-md-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-11 { grid-column-start:11; }
   * }}}
  */
  def `g-start-md-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-2 { grid-column-start:2; }
   * }}}
  */
  def `g-start-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-3 { grid-column-start:3; }
   * }}}
  */
  def `g-start-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-4 { grid-column-start:4; }
   * }}}
  */
  def `g-start-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-5 { grid-column-start:5; }
   * }}}
  */
  def `g-start-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-6 { grid-column-start:6; }
   * }}}
  */
  def `g-start-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-7 { grid-column-start:7; }
   * }}}
  */
  def `g-start-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-8 { grid-column-start:8; }
   * }}}
  */
  def `g-start-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-md-9 { grid-column-start:9; }
   * }}}
  */
  def `g-start-md-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-1 { grid-column-start:1; }
   * }}}
  */
  def `g-start-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-10 { grid-column-start:10; }
   * }}}
  */
  def `g-start-sm-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-11 { grid-column-start:11; }
   * }}}
  */
  def `g-start-sm-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-2 { grid-column-start:2; }
   * }}}
  */
  def `g-start-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-3 { grid-column-start:3; }
   * }}}
  */
  def `g-start-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-4 { grid-column-start:4; }
   * }}}
  */
  def `g-start-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-5 { grid-column-start:5; }
   * }}}
  */
  def `g-start-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-6 { grid-column-start:6; }
   * }}}
  */
  def `g-start-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-7 { grid-column-start:7; }
   * }}}
  */
  def `g-start-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-8 { grid-column-start:8; }
   * }}}
  */
  def `g-start-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-sm-9 { grid-column-start:9; }
   * }}}
  */
  def `g-start-sm-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-1 { grid-column-start:1; }
   * }}}
  */
  def `g-start-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-10 { grid-column-start:10; }
   * }}}
  */
  def `g-start-xl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-11 { grid-column-start:11; }
   * }}}
  */
  def `g-start-xl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-2 { grid-column-start:2; }
   * }}}
  */
  def `g-start-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-3 { grid-column-start:3; }
   * }}}
  */
  def `g-start-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-4 { grid-column-start:4; }
   * }}}
  */
  def `g-start-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-5 { grid-column-start:5; }
   * }}}
  */
  def `g-start-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-6 { grid-column-start:6; }
   * }}}
  */
  def `g-start-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-7 { grid-column-start:7; }
   * }}}
  */
  def `g-start-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-8 { grid-column-start:8; }
   * }}}
  */
  def `g-start-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xl-9 { grid-column-start:9; }
   * }}}
  */
  def `g-start-xl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-1 { grid-column-start:1; }
   * }}}
  */
  def `g-start-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-10 { grid-column-start:10; }
   * }}}
  */
  def `g-start-xxl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-11 { grid-column-start:11; }
   * }}}
  */
  def `g-start-xxl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-2 { grid-column-start:2; }
   * }}}
  */
  def `g-start-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-3 { grid-column-start:3; }
   * }}}
  */
  def `g-start-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-4 { grid-column-start:4; }
   * }}}
  */
  def `g-start-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-5 { grid-column-start:5; }
   * }}}
  */
  def `g-start-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-6 { grid-column-start:6; }
   * }}}
  */
  def `g-start-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-7 { grid-column-start:7; }
   * }}}
  */
  def `g-start-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-8 { grid-column-start:8; }
   * }}}
  */
  def `g-start-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid .g-start-xxl-9 { grid-column-start:9; }
   * }}}
  */
  def `g-start-xxl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-0,
   * .gy-xl-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `g-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-1,
   * .gy-xl-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `g-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-2,
   * .gy-xl-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `g-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-3,
   * .gy-xl-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `g-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-4,
   * .gy-xl-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `g-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-5,
   * .gy-xl-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `g-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-6,
   * .gy-xl-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `g-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-7,
   * .gy-xl-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `g-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-8,
   * .gy-xl-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `g-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-0,
   * .gy-xxl-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `g-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-1,
   * .gy-xxl-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `g-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-2,
   * .gy-xxl-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `g-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-3,
   * .gy-xxl-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `g-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-4,
   * .gy-xxl-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `g-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-5,
   * .gy-xxl-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `g-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-6,
   * .gy-xxl-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `g-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-7,
   * .gy-xxl-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `g-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-8,
   * .gy-xxl-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `g-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-0 { gap:0 !important; }
   * }}}
  */
  def `gap-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-1 { gap:.25rem !important; }
   * }}}
  */
  def `gap-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-2 { gap:.5rem !important; }
   * }}}
  */
  def `gap-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-3 { gap:1rem !important; }
   * }}}
  */
  def `gap-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-4 { gap:1.5rem !important; }
   * }}}
  */
  def `gap-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-5 { gap:2rem !important; }
   * }}}
  */
  def `gap-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-6 { gap:3rem !important; }
   * }}}
  */
  def `gap-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-7 { gap:5rem !important; }
   * }}}
  */
  def `gap-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-8 { gap:8rem !important; }
   * }}}
  */
  def `gap-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-0 { gap:0 !important; }
   * }}}
  */
  def `gap-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-1 { gap:.25rem !important; }
   * }}}
  */
  def `gap-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-2 { gap:.5rem !important; }
   * }}}
  */
  def `gap-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-3 { gap:1rem !important; }
   * }}}
  */
  def `gap-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-4 { gap:1.5rem !important; }
   * }}}
  */
  def `gap-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-5 { gap:2rem !important; }
   * }}}
  */
  def `gap-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-6 { gap:3rem !important; }
   * }}}
  */
  def `gap-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-7 { gap:5rem !important; }
   * }}}
  */
  def `gap-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-lg-8 { gap:8rem !important; }
   * }}}
  */
  def `gap-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-0 { gap:0 !important; }
   * }}}
  */
  def `gap-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-1 { gap:.25rem !important; }
   * }}}
  */
  def `gap-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-2 { gap:.5rem !important; }
   * }}}
  */
  def `gap-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-3 { gap:1rem !important; }
   * }}}
  */
  def `gap-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-4 { gap:1.5rem !important; }
   * }}}
  */
  def `gap-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-5 { gap:2rem !important; }
   * }}}
  */
  def `gap-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-6 { gap:3rem !important; }
   * }}}
  */
  def `gap-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-7 { gap:5rem !important; }
   * }}}
  */
  def `gap-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-md-8 { gap:8rem !important; }
   * }}}
  */
  def `gap-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-0 { gap:0 !important; }
   * }}}
  */
  def `gap-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-1 { gap:.25rem !important; }
   * }}}
  */
  def `gap-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-2 { gap:.5rem !important; }
   * }}}
  */
  def `gap-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-3 { gap:1rem !important; }
   * }}}
  */
  def `gap-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-4 { gap:1.5rem !important; }
   * }}}
  */
  def `gap-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-5 { gap:2rem !important; }
   * }}}
  */
  def `gap-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-6 { gap:3rem !important; }
   * }}}
  */
  def `gap-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-7 { gap:5rem !important; }
   * }}}
  */
  def `gap-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-sm-8 { gap:8rem !important; }
   * }}}
  */
  def `gap-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-0 { gap:0 !important; }
   * }}}
  */
  def `gap-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-1 { gap:.25rem !important; }
   * }}}
  */
  def `gap-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-2 { gap:.5rem !important; }
   * }}}
  */
  def `gap-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-3 { gap:1rem !important; }
   * }}}
  */
  def `gap-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-4 { gap:1.5rem !important; }
   * }}}
  */
  def `gap-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-5 { gap:2rem !important; }
   * }}}
  */
  def `gap-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-6 { gap:3rem !important; }
   * }}}
  */
  def `gap-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-7 { gap:5rem !important; }
   * }}}
  */
  def `gap-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xl-8 { gap:8rem !important; }
   * }}}
  */
  def `gap-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-0 { gap:0 !important; }
   * }}}
  */
  def `gap-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-1 { gap:.25rem !important; }
   * }}}
  */
  def `gap-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-2 { gap:.5rem !important; }
   * }}}
  */
  def `gap-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-3 { gap:1rem !important; }
   * }}}
  */
  def `gap-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-4 { gap:1.5rem !important; }
   * }}}
  */
  def `gap-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-5 { gap:2rem !important; }
   * }}}
  */
  def `gap-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-6 { gap:3rem !important; }
   * }}}
  */
  def `gap-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-7 { gap:5rem !important; }
   * }}}
  */
  def `gap-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .gap-xxl-8 { gap:8rem !important; }
   * }}}
  */
  def `gap-xxl-8`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `gl-active`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * [data-star-rating] :not(.gl-active)>.gl-star-full { color:var(--gl-star-color-inactive) !important; }
   * }}}
  */
  def `gl-star-full`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .grid {
   *   display:grid;
   *   grid-template-rows:repeat(var(--tblr-rows,1),1fr);
   *   grid-template-columns:repeat(var(--tblr-columns,12),1fr);
   *   gap:var(--tblr-gap,var(--tblr-page-padding));
   * }
   * }}}
  */
  def grid: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-0,
   * .gx-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `gx-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-1,
   * .gx-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `gx-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-2,
   * .gx-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `gx-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-3,
   * .gx-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `gx-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-4,
   * .gx-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `gx-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-5,
   * .gx-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `gx-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-6,
   * .gx-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `gx-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-7,
   * .gx-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `gx-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-8,
   * .gx-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `gx-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-0,
   * .gx-lg-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `gx-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-1,
   * .gx-lg-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `gx-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-2,
   * .gx-lg-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `gx-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-3,
   * .gx-lg-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `gx-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-4,
   * .gx-lg-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `gx-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-5,
   * .gx-lg-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `gx-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-6,
   * .gx-lg-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `gx-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-7,
   * .gx-lg-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `gx-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-8,
   * .gx-lg-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `gx-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-0,
   * .gx-md-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `gx-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-1,
   * .gx-md-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `gx-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-2,
   * .gx-md-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `gx-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-3,
   * .gx-md-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `gx-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-4,
   * .gx-md-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `gx-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-5,
   * .gx-md-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `gx-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-6,
   * .gx-md-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `gx-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-7,
   * .gx-md-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `gx-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-8,
   * .gx-md-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `gx-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-0,
   * .gx-sm-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `gx-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-1,
   * .gx-sm-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `gx-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-2,
   * .gx-sm-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `gx-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-3,
   * .gx-sm-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `gx-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-4,
   * .gx-sm-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `gx-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-5,
   * .gx-sm-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `gx-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-6,
   * .gx-sm-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `gx-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-7,
   * .gx-sm-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `gx-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-8,
   * .gx-sm-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `gx-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-0,
   * .gx-xl-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `gx-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-1,
   * .gx-xl-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `gx-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-2,
   * .gx-xl-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `gx-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-3,
   * .gx-xl-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `gx-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-4,
   * .gx-xl-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `gx-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-5,
   * .gx-xl-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `gx-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-6,
   * .gx-xl-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `gx-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-7,
   * .gx-xl-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `gx-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-8,
   * .gx-xl-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `gx-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-0,
   * .gx-xxl-0 { --tblr-gutter-x:0; }
   * }}}
  */
  def `gx-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-1,
   * .gx-xxl-1 { --tblr-gutter-x:0.25rem; }
   * }}}
  */
  def `gx-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-2,
   * .gx-xxl-2 { --tblr-gutter-x:0.5rem; }
   * }}}
  */
  def `gx-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-3,
   * .gx-xxl-3 { --tblr-gutter-x:1rem; }
   * }}}
  */
  def `gx-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-4,
   * .gx-xxl-4 { --tblr-gutter-x:1.5rem; }
   * }}}
  */
  def `gx-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-5,
   * .gx-xxl-5 { --tblr-gutter-x:2rem; }
   * }}}
  */
  def `gx-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-6,
   * .gx-xxl-6 { --tblr-gutter-x:3rem; }
   * }}}
  */
  def `gx-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-7,
   * .gx-xxl-7 { --tblr-gutter-x:5rem; }
   * }}}
  */
  def `gx-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-8,
   * .gx-xxl-8 { --tblr-gutter-x:8rem; }
   * }}}
  */
  def `gx-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-0,
   * .gy-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `gy-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-1,
   * .gy-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `gy-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-2,
   * .gy-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `gy-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-3,
   * .gy-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `gy-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-4,
   * .gy-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `gy-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-5,
   * .gy-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `gy-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-6,
   * .gy-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `gy-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-7,
   * .gy-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `gy-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-8,
   * .gy-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `gy-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-0,
   * .gy-lg-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `gy-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-1,
   * .gy-lg-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `gy-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-2,
   * .gy-lg-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `gy-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-3,
   * .gy-lg-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `gy-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-4,
   * .gy-lg-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `gy-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-5,
   * .gy-lg-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `gy-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-6,
   * .gy-lg-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `gy-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-7,
   * .gy-lg-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `gy-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-lg-8,
   * .gy-lg-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `gy-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-0,
   * .gy-md-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `gy-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-1,
   * .gy-md-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `gy-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-2,
   * .gy-md-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `gy-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-3,
   * .gy-md-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `gy-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-4,
   * .gy-md-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `gy-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-5,
   * .gy-md-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `gy-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-6,
   * .gy-md-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `gy-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-7,
   * .gy-md-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `gy-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-md-8,
   * .gy-md-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `gy-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-0,
   * .gy-sm-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `gy-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-1,
   * .gy-sm-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `gy-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-2,
   * .gy-sm-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `gy-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-3,
   * .gy-sm-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `gy-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-4,
   * .gy-sm-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `gy-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-5,
   * .gy-sm-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `gy-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-6,
   * .gy-sm-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `gy-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-7,
   * .gy-sm-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `gy-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-sm-8,
   * .gy-sm-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `gy-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-0,
   * .gy-xl-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `gy-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-1,
   * .gy-xl-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `gy-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-2,
   * .gy-xl-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `gy-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-3,
   * .gy-xl-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `gy-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-4,
   * .gy-xl-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `gy-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-5,
   * .gy-xl-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `gy-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-6,
   * .gy-xl-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `gy-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-7,
   * .gy-xl-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `gy-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xl-8,
   * .gy-xl-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `gy-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-0,
   * .gy-xxl-0 { --tblr-gutter-y:0; }
   * }}}
  */
  def `gy-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-1,
   * .gy-xxl-1 { --tblr-gutter-y:0.25rem; }
   * }}}
  */
  def `gy-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-2,
   * .gy-xxl-2 { --tblr-gutter-y:0.5rem; }
   * }}}
  */
  def `gy-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-3,
   * .gy-xxl-3 { --tblr-gutter-y:1rem; }
   * }}}
  */
  def `gy-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-4,
   * .gy-xxl-4 { --tblr-gutter-y:1.5rem; }
   * }}}
  */
  def `gy-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-5,
   * .gy-xxl-5 { --tblr-gutter-y:2rem; }
   * }}}
  */
  def `gy-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-6,
   * .gy-xxl-6 { --tblr-gutter-y:3rem; }
   * }}}
  */
  def `gy-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-7,
   * .gy-xxl-7 { --tblr-gutter-y:5rem; }
   * }}}
  */
  def `gy-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .g-xxl-8,
   * .gy-xxl-8 { --tblr-gutter-y:8rem; }
   * }}}
  */
  def `gy-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-0 { height:0 !important; }
   * }}}
  */
  def `h-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-1 { height:.25rem !important; }
   * }}}
  */
  def `h-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-100 { height:100% !important; }
   * }}}
  */
  def `h-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-2 { height:.5rem !important; }
   * }}}
  */
  def `h-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-25 { height:25% !important; }
   * }}}
  */
  def `h-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-3 { height:1rem !important; }
   * }}}
  */
  def `h-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-33 { height:33.33333% !important; }
   * }}}
  */
  def `h-33`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-4 { height:1.5rem !important; }
   * }}}
  */
  def `h-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-5 { height:2rem !important; }
   * }}}
  */
  def `h-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-50 { height:50% !important; }
   * }}}
  */
  def `h-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-6 { height:3rem !important; }
   * }}}
  */
  def `h-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-66 { height:66.66666% !important; }
   * }}}
  */
  def `h-66`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-7 { height:5rem !important; }
   * }}}
  */
  def `h-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-75 { height:75% !important; }
   * }}}
  */
  def `h-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-8 { height:8rem !important; }
   * }}}
  */
  def `h-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-auto { height:auto !important; }
   * }}}
  */
  def `h-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-full { height:100% !important; }
   * }}}
  */
  def `h-full`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h-px { height:1px !important; }
   * }}}
  */
  def `h-px`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h1,
   * .h2,
   * .h3,
   * .h4,
   * .h5,
   * .h6,
   * h1,
   * h2,
   * h3,
   * h4,
   * h5,
   * h6 {
   *   margin-top:0;
   *   margin-bottom:var(--tblr-spacer);
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1.2;
   *   color:var(--tblr-heading-color);
   * }
   * }}}
  */
  def h1: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h1,
   * .h2,
   * .h3,
   * .h4,
   * .h5,
   * .h6,
   * h1,
   * h2,
   * h3,
   * h4,
   * h5,
   * h6 {
   *   margin-top:0;
   *   margin-bottom:var(--tblr-spacer);
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1.2;
   *   color:var(--tblr-heading-color);
   * }
   * }}}
  */
  def h2: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h1,
   * .h2,
   * .h3,
   * .h4,
   * .h5,
   * .h6,
   * h1,
   * h2,
   * h3,
   * h4,
   * h5,
   * h6 {
   *   margin-top:0;
   *   margin-bottom:var(--tblr-spacer);
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1.2;
   *   color:var(--tblr-heading-color);
   * }
   * }}}
  */
  def h3: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h1,
   * .h2,
   * .h3,
   * .h4,
   * .h5,
   * .h6,
   * h1,
   * h2,
   * h3,
   * h4,
   * h5,
   * h6 {
   *   margin-top:0;
   *   margin-bottom:var(--tblr-spacer);
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1.2;
   *   color:var(--tblr-heading-color);
   * }
   * }}}
  */
  def h4: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h1,
   * .h2,
   * .h3,
   * .h4,
   * .h5,
   * .h6,
   * h1,
   * h2,
   * h3,
   * h4,
   * h5,
   * h6 {
   *   margin-top:0;
   *   margin-bottom:var(--tblr-spacer);
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1.2;
   *   color:var(--tblr-heading-color);
   * }
   * }}}
  */
  def h5: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .h1,
   * .h2,
   * .h3,
   * .h4,
   * .h5,
   * .h6,
   * h1,
   * h2,
   * h3,
   * h4,
   * h5,
   * h6 {
   *   margin-top:0;
   *   margin-bottom:var(--tblr-spacer);
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1.2;
   *   color:var(--tblr-heading-color);
   * }
   * }}}
  */
  def h6: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-clear_button.focus.has-items .clear-button,
   * .plugin-clear_button:not(.disabled):hover.has-items .clear-button { opacity:1; }
   * }}}
  */
  def `has-items`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `has-validation`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * body.theme-dark .hide-theme-dark,
   * body[data-bs-theme=dark] .hide-theme-dark { display:none !important; }
   * }}}
  */
  def `hide-theme-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * body:not(.theme-dark):not([data-bs-theme=dark]) .hide-theme-light { display:none !important; }
   * }}}
  */
  def `hide-theme-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-sm.hiding,
   * .offcanvas-sm.show,
   * .offcanvas-sm.showing { visibility:visible; }
   * }}}
  */
  def hiding: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css;tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .highlight pre,
   * pre.highlight {
   *   max-height:30rem;
   *   margin:1.5rem 0;
   *   overflow:auto;
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def highlight: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .scrollable.hover { overflow-y:hidden; }
   * }}}
  */
  def hover: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hover-shadow:hover { box-shadow:rgba(var(--tblr-body-color-rgb),.04) 0 2px 4px 0 !important; }
   * }}}
  */
  def `hover-shadow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hover-shadow-lg:hover { box-shadow:0 1rem 3rem rgba(0,0,0,.175) !important; }
   * }}}
  */
  def `hover-shadow-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hover-shadow-none:hover { box-shadow:none !important; }
   * }}}
  */
  def `hover-shadow-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hover-shadow-sm:hover { box-shadow:0 .125rem .25rem rgba(0,0,0,.075) !important; }
   * }}}
  */
  def `hover-shadow-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hr,
   * hr {
   *   margin:2rem 0;
   *   color:inherit;
   *   border:0;
   *   border-top:var(--tblr-border-width) solid;
   *   opacity:.16;
   * }
   * }}}
  */
  def hr: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hr-text {
   *   display:flex;
   *   align-items:center;
   *   margin:2rem 0;
   *   font-size:.625rem;
   *   font-weight:var(--tblr-font-weight-bold);
   *   text-transform:uppercase;
   *   letter-spacing:.04em;
   *   line-height:1rem;
   *   color:var(--tblr-secondary);
   *   height:1px;
   * }
   * }}}
  */
  def `hr-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hr-text.hr-text-left:before { content:none; }
   * }}}
  */
  def `hr-text-left`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hr-text.hr-text-right:before { content:""; }
   * }}}
  */
  def `hr-text-right`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hr-text-spaceless { margin:-.5rem 0; }
   * }}}
  */
  def `hr-text-spaceless`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .hstack {
   *   display:flex;
   *   flex-direction:row;
   *   align-items:center;
   *   align-self:stretch;
   * }
   * }}}
  */
  def hstack: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .demo-icons-list-item .icon {
   *   width:1.5rem;
   *   height:1.5rem;
   *   font-size:1.5rem;
   * }
   * }}}
  */
  def icon: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-filled { fill:currentColor; }
   * }}}
  */
  def `icon-filled`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-inline {
   *   --tblr-icon-size:1rem;
   *   vertical-align:-.2rem;
   * }
   * }}}
  */
  def `icon-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-lg {
   *   --tblr-icon-size:3.5rem;
   *   stroke-width:1;
   * }
   * }}}
  */
  def `icon-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-link {
   *   display:inline-flex;
   *   gap:.375rem;
   *   align-items:center;
   *   -webkit-text-decoration-color:rgba(var(--tblr-link-color-rgb),var(--tblr-link-opacity,.5));
   *   text-decoration-color:rgba(var(--tblr-link-color-rgb),var(--tblr-link-opacity,.5));
   *   text-underline-offset:.25em;
   *   -webkit-backface-visibility:hidden;
   *   backface-visibility:hidden;
   * }
   * }}}
  */
  def `icon-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-link-hover:focus-visible>.bi,
   * .icon-link-hover:hover>.bi { transform:var(--tblr-icon-link-transform,translate3d(.25em,0,0)); }
   * }}}
  */
  def `icon-link-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-md {
   *   --tblr-icon-size:2.5rem;
   *   stroke-width:1;
   * }
   * }}}
  */
  def `icon-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-pulse {
   *   transition:all .15s ease 0s;
   *   animation:pulse 2s ease infinite;
   *   animation-fill-mode:both;
   * }
   * }}}
  */
  def `icon-pulse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn .icon-right { margin:0 calc(var(--tblr-btn-padding-x)/-4) 0 calc(var(--tblr-btn-padding-x)/2); }
   * }}}
  */
  def `icon-right`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-rotate {
   *   transition:all .15s ease 0s;
   *   animation:rotate-360 3s linear infinite;
   *   animation-fill-mode:both;
   * }
   * }}}
  */
  def `icon-rotate`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-sm {
   *   --tblr-icon-size:1rem;
   *   stroke-width:1;
   * }
   * }}}
  */
  def `icon-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .icon-tada {
   *   transition:all .15s ease 0s;
   *   animation:tada 3s ease infinite;
   *   animation-fill-mode:both;
   * }
   * }}}
  */
  def `icon-tada`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-fluid {
   *   max-width:100%;
   *   height:auto;
   * }
   * }}}
  */
  def `img-fluid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive {
   *   --tblr-img-responsive-ratio:75%;
   *   background:no-repeat center/cover;
   *   padding-top:var(--tblr-img-responsive-ratio);
   * }
   * }}}
  */
  def `img-responsive`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-16x9 { --tblr-img-responsive-ratio:56.25%; }
   * }}}
  */
  def `img-responsive-16x9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-1x1 { --tblr-img-responsive-ratio:100%; }
   * }}}
  */
  def `img-responsive-1x1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-1x2 { --tblr-img-responsive-ratio:200%; }
   * }}}
  */
  def `img-responsive-1x2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-1x3 { --tblr-img-responsive-ratio:300%; }
   * }}}
  */
  def `img-responsive-1x3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-21x9 { --tblr-img-responsive-ratio:42.8571428571%; }
   * }}}
  */
  def `img-responsive-21x9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-2x1 { --tblr-img-responsive-ratio:50%; }
   * }}}
  */
  def `img-responsive-2x1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-3x1 { --tblr-img-responsive-ratio:33.3333333333%; }
   * }}}
  */
  def `img-responsive-3x1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-3x4 { --tblr-img-responsive-ratio:133.3333333333%; }
   * }}}
  */
  def `img-responsive-3x4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-4x3 { --tblr-img-responsive-ratio:75%; }
   * }}}
  */
  def `img-responsive-4x3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-9x16 { --tblr-img-responsive-ratio:177.7777777778%; }
   * }}}
  */
  def `img-responsive-9x16`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-9x21 { --tblr-img-responsive-ratio:233.3333333333%; }
   * }}}
  */
  def `img-responsive-9x21`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-responsive-grid { padding-top:calc(var(--tblr-img-responsive-ratio) - var(--tblr-gutter-y)/2); }
   * }}}
  */
  def `img-responsive-grid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .img-thumbnail {
   *   padding:.25rem;
   *   background-color:var(--tblr-body-bg);
   *   border:var(--tblr-border-width) solid var(--tblr-border-color);
   *   border-radius:var(--tblr-border-radius);
   *   box-shadow:var(--tblr-box-shadow-sm);
   *   max-width:100%;
   *   height:auto;
   * }
   * }}}
  */
  def `img-thumbnail`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .initialism {
   *   font-size:85.714285%;
   *   text-transform:uppercase;
   * }
   * }}}
  */
  def initialism: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-control,
   * .ts-wrapper.single.input-active .ts-control {
   *   background:var(--tblr-bg-forms);
   *   cursor:text;
   * }
   * }}}
  */
  def `input-active`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group>.ts-wrapper { flex-grow:1; }
   * }}}
  */
  def `input-group`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-flat:focus-within {
   *   box-shadow:0 0 0 .25rem rgba(var(--tblr-primary-rgb),.25);
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def `input-group-flat`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-lg>.ts-wrapper,
   * .ts-wrapper.form-control-lg,
   * .ts-wrapper.form-select-lg { min-height:calc(1.4285714286em + 1rem + calc(var(--tblr-border-width)*2)); }
   * }}}
  */
  def `input-group-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-link { font-size:.75rem; }
   * }}}
  */
  def `input-group-link`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-sm>.ts-wrapper,
   * .ts-wrapper.form-control-sm,
   * .ts-wrapper.form-select-sm { min-height:calc(1.4285714286em + .25rem + calc(var(--tblr-border-width)*2)); }
   * }}}
  */
  def `input-group-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-group-text {
   *   display:flex;
   *   align-items:center;
   *   padding:.5625rem .75rem;
   *   font-size:.875rem;
   *   font-weight:400;
   *   line-height:1.4285714286;
   *   color:var(--tblr-secondary);
   *   text-align:center;
   *   white-space:nowrap;
   *   background-color:var(--tblr-bg-surface-secondary);
   *   border:var(--tblr-border-width) solid var(--tblr-border-color);
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def `input-group-text`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .input-hidden .ts-control>input {
   *   opacity:0;
   *   position:absolute;
   *   left:-10000px;
   * }
   * }}}
  */
  def `input-hidden`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-icon { position:relative; }
   * }}}
  */
  def `input-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .input-icon-addon {
   *   position:absolute;
   *   top:0;
   *   bottom:0;
   *   left:0;
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   min-width:2.5rem;
   *   color:var(--tblr-icon-color);
   *   pointer-events:none;
   *   font-size:1.2em;
   * }
   * }}}
  */
  def `input-icon-addon`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.is-invalid,
   * .was-validated .invalid,
   * .was-validated :invalid+.ts-wrapper { border-color:var(--tblr-form-invalid-color); }
   * }}}
  */
  def invalid: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `invalid-feedback`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `invalid-tooltip`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .invisible { visibility:hidden !important; }
   * }}}
  */
  def invisible: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-wrapper.is-invalid,
   * .was-validated .invalid,
   * .was-validated :invalid+.ts-wrapper { border-color:var(--tblr-form-invalid-color); }
   * }}}
  */
  def `is-invalid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control.is-invalid-lite,
   * .form-control.is-valid-lite,
   * .form-select.is-invalid-lite,
   * .form-select.is-valid-lite { border-color:var(--tblr-border-color) !important; }
   * }}}
  */
  def `is-invalid-lite`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-wrapper.is-valid,
   * .was-validated .valid,
   * .was-validated :valid+.ts-wrapper { border-color:var(--tblr-form-valid-color); }
   * }}}
  */
  def `is-valid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .form-control.is-invalid-lite,
   * .form-control.is-valid-lite,
   * .form-select.is-invalid-lite,
   * .form-select.is-valid-lite { border-color:var(--tblr-border-color) !important; }
   * }}}
  */
  def `is-valid-lite`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-remove_button .item {
   *   display:inline-flex;
   *   align-items:center;
   *   padding-right:0 !important;
   * }
   * }}}
  */
  def item: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-dropdown_input .items-placeholder {
   *   border:0 none !important;
   *   box-shadow:none !important;
   *   width:100%;
   * }
   * }}}
  */
  def `items-placeholder`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-around { justify-content:space-around !important; }
   * }}}
  */
  def `justify-content-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-between { justify-content:space-between !important; }
   * }}}
  */
  def `justify-content-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-center { justify-content:center !important; }
   * }}}
  */
  def `justify-content-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-end { justify-content:flex-end !important; }
   * }}}
  */
  def `justify-content-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-evenly { justify-content:space-evenly !important; }
   * }}}
  */
  def `justify-content-evenly`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-lg-around { justify-content:space-around !important; }
   * }}}
  */
  def `justify-content-lg-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-lg-between { justify-content:space-between !important; }
   * }}}
  */
  def `justify-content-lg-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-lg-center { justify-content:center !important; }
   * }}}
  */
  def `justify-content-lg-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-lg-end { justify-content:flex-end !important; }
   * }}}
  */
  def `justify-content-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-lg-evenly { justify-content:space-evenly !important; }
   * }}}
  */
  def `justify-content-lg-evenly`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-lg-start { justify-content:flex-start !important; }
   * }}}
  */
  def `justify-content-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-md-around { justify-content:space-around !important; }
   * }}}
  */
  def `justify-content-md-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-md-between { justify-content:space-between !important; }
   * }}}
  */
  def `justify-content-md-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-md-center { justify-content:center !important; }
   * }}}
  */
  def `justify-content-md-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-md-end { justify-content:flex-end !important; }
   * }}}
  */
  def `justify-content-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-md-evenly { justify-content:space-evenly !important; }
   * }}}
  */
  def `justify-content-md-evenly`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-md-start { justify-content:flex-start !important; }
   * }}}
  */
  def `justify-content-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-sm-around { justify-content:space-around !important; }
   * }}}
  */
  def `justify-content-sm-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-sm-between { justify-content:space-between !important; }
   * }}}
  */
  def `justify-content-sm-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-sm-center { justify-content:center !important; }
   * }}}
  */
  def `justify-content-sm-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-sm-end { justify-content:flex-end !important; }
   * }}}
  */
  def `justify-content-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-sm-evenly { justify-content:space-evenly !important; }
   * }}}
  */
  def `justify-content-sm-evenly`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-sm-start { justify-content:flex-start !important; }
   * }}}
  */
  def `justify-content-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-start { justify-content:flex-start !important; }
   * }}}
  */
  def `justify-content-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xl-around { justify-content:space-around !important; }
   * }}}
  */
  def `justify-content-xl-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xl-between { justify-content:space-between !important; }
   * }}}
  */
  def `justify-content-xl-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xl-center { justify-content:center !important; }
   * }}}
  */
  def `justify-content-xl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xl-end { justify-content:flex-end !important; }
   * }}}
  */
  def `justify-content-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xl-evenly { justify-content:space-evenly !important; }
   * }}}
  */
  def `justify-content-xl-evenly`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xl-start { justify-content:flex-start !important; }
   * }}}
  */
  def `justify-content-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xxl-around { justify-content:space-around !important; }
   * }}}
  */
  def `justify-content-xxl-around`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xxl-between { justify-content:space-between !important; }
   * }}}
  */
  def `justify-content-xxl-between`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xxl-center { justify-content:center !important; }
   * }}}
  */
  def `justify-content-xxl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xxl-end { justify-content:flex-end !important; }
   * }}}
  */
  def `justify-content-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xxl-evenly { justify-content:space-evenly !important; }
   * }}}
  */
  def `justify-content-xxl-evenly`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .justify-content-xxl-start { justify-content:flex-start !important; }
   * }}}
  */
  def `justify-content-xxl-start`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-container {
   *   touch-action:none;
   *   position:relative;
   *   overflow:hidden;
   *   height:100%;
   *   width:100%;
   * }
   * }}}
  */
  def `jvm-container`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container .jvm-legend {
   *   background-color:#fff;
   *   border:1px solid #e5e7eb;
   *   margin-left:.75rem;
   *   border-radius:.25rem;
   *   border-color:#e5e7eb;
   *   padding:.6rem;
   *   box-shadow:0 1px 2px 0 rgba(0,0,0,.05);
   *   float:left;
   * }
   * }}}
  */
  def `jvm-legend`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container .jvm-legend .jvm-legend-inner { overflow:hidden; }
   * }}}
  */
  def `jvm-legend-inner`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container .jvm-legend .jvm-legend-inner .jvm-legend-tick {
   *   overflow:hidden;
   *   min-width:40px;
   * }
   * }}}
  */
  def `jvm-legend-tick`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container .jvm-legend .jvm-legend-inner .jvm-legend-tick .jvm-legend-tick-sample {
   *   border-radius:4px;
   *   margin-right:.65rem;
   *   height:16px;
   *   width:16px;
   *   float:left;
   * }
   * }}}
  */
  def `jvm-legend-tick-sample`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container .jvm-legend .jvm-legend-inner .jvm-legend-tick .jvm-legend-tick-text {
   *   font-size:12px;
   *   text-align:center;
   *   float:left;
   * }
   * }}}
  */
  def `jvm-legend-tick-text`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container .jvm-legend .jvm-legend-title {
   *   line-height:1;
   *   border-bottom:1px solid #e5e7eb;
   *   padding-bottom:.5rem;
   *   margin-bottom:.575rem;
   *   text-align:left;
   * }
   * }}}
  */
  def `jvm-legend-title`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-line[animation=true] { animation:jvm-line-animation 10s linear forwards infinite; }
   * }}}
  */
  def `jvm-line`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container {
   *   right:15px;
   *   position:absolute;
   * }
   * }}}
  */
  def `jvm-series-container`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container.jvm-series-h { bottom:15px; }
   * }}}
  */
  def `jvm-series-h`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-series-container.jvm-series-v { top:15px; }
   * }}}
  */
  def `jvm-series-v`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-tooltip {
   *   border-radius:3px;
   *   background-color:#5c5cff;
   *   font-family:sans-serif,Verdana;
   *   font-size:smaller;
   *   box-shadow:1px 2px 12px rgba(0,0,0,.2);
   *   padding:3px 5px;
   *   white-space:nowrap;
   *   position:absolute;
   *   display:none;
   *   color:#fff;
   * }
   * }}}
  */
  def `jvm-tooltip`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-zoom-btn {
   *   border-radius:3px;
   *   background-color:#292929;
   *   padding:3px;
   *   box-sizing:border-box;
   *   position:absolute;
   *   line-height:10px;
   *   cursor:pointer;
   *   color:#fff;
   *   height:15px;
   *   width:15px;
   *   left:10px;
   * }
   * }}}
  */
  def `jvm-zoom-btn`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-zoomin,
   * .jvm-zoomout,
   * image,
   * text {
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   * }
   * }}}
  */
  def `jvm-zoomin`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .jvm-zoomin,
   * .jvm-zoomout,
   * image,
   * text {
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   * }
   * }}}
  */
  def `jvm-zoomout`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .k { color:#93ddfd; }
   * }}}
  */
  def k: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .kbd,
   * kbd {
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   display:inline-block;
   *   box-sizing:border-box;
   *   max-width:100%;
   *   font-size:var(--tblr-font-size-h5);
   *   font-weight:var(--tblr-font-weight-medium);
   *   line-height:1;
   *   vertical-align:baseline;
   *   border-radius:var(--tblr-border-radius);
   * }
   * }}}
  */
  def kbd: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .layout-boxed {
   *   --tblr-theme-boxed-border-radius:0;
   *   --tblr-theme-boxed-width:1320px;
   * }
   * }}}
  */
  def `layout-boxed`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .layout-fluid .container,
   * .layout-fluid [class*=" container-"],
   * .layout-fluid [class^=container-] { max-width:100%; }
   * }}}
  */
  def `layout-fluid`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .lead {
   *   font-size:.875rem;
   *   font-weight:var(--tblr-font-weight-normal);
   * }
   * }}}
  */
  def lead: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .legend {
   *   --tblr-legend-size:0.75em;
   *   display:inline-block;
   *   background:var(--tblr-border-color);
   *   width:var(--tblr-legend-size);
   *   height:var(--tblr-legend-size);
   *   border-radius:var(--tblr-border-radius-sm);
   *   border:1px solid var(--tblr-border-color-translucent);
   * }
   * }}}
  */
  def legend: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .lh-1 { line-height:1 !important; }
   * }}}
  */
  def `lh-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .lh-base { line-height:1.4285714286 !important; }
   * }}}
  */
  def `lh-base`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .lh-lg { line-height:1.7142857143 !important; }
   * }}}
  */
  def `lh-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .lh-sm { line-height:1.1428571429 !important; }
   * }}}
  */
  def `lh-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-azure {
   *   color:RGBA(var(--tblr-azure-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-azure-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-azure-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-bitbucket {
   *   color:RGBA(var(--tblr-bitbucket-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-bitbucket-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-bitbucket-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-blue {
   *   color:RGBA(var(--tblr-blue-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-blue-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-blue-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-body-emphasis {
   *   color:RGBA(var(--tblr-emphasis-color-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-emphasis-color-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-emphasis-color-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-body-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-cyan {
   *   color:RGBA(var(--tblr-cyan-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-cyan-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-cyan-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-danger {
   *   color:RGBA(var(--tblr-danger-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-danger-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-danger-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-dark {
   *   color:RGBA(var(--tblr-dark-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-dark-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-dark-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-dribbble {
   *   color:RGBA(var(--tblr-dribbble-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-dribbble-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-dribbble-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-facebook {
   *   color:RGBA(var(--tblr-facebook-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-facebook-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-facebook-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-flickr {
   *   color:RGBA(var(--tblr-flickr-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-flickr-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-flickr-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-github {
   *   color:RGBA(var(--tblr-github-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-github-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-github-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-google {
   *   color:RGBA(var(--tblr-google-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-google-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-google-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-green {
   *   color:RGBA(var(--tblr-green-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-green-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-green-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-indigo {
   *   color:RGBA(var(--tblr-indigo-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-indigo-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-indigo-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-info {
   *   color:RGBA(var(--tblr-info-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-info-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-info-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-instagram {
   *   color:RGBA(var(--tblr-instagram-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-instagram-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-instagram-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-light {
   *   color:RGBA(var(--tblr-light-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-light-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-light-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-lime {
   *   color:RGBA(var(--tblr-lime-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-lime-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-lime-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-linkedin {
   *   color:RGBA(var(--tblr-linkedin-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-linkedin-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-linkedin-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-muted {
   *   color:RGBA(var(--tblr-muted-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-muted-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-muted-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-offset-1 { text-underline-offset:.125em !important; }
   * }}}
  */
  def `link-offset-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-offset-1-hover:hover { text-underline-offset:.125em !important; }
   * }}}
  */
  def `link-offset-1-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-offset-2 { text-underline-offset:.25em !important; }
   * }}}
  */
  def `link-offset-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-offset-2-hover:hover { text-underline-offset:.25em !important; }
   * }}}
  */
  def `link-offset-2-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-offset-3 { text-underline-offset:.375em !important; }
   * }}}
  */
  def `link-offset-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-offset-3-hover:hover { text-underline-offset:.375em !important; }
   * }}}
  */
  def `link-offset-3-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-10 { --tblr-link-opacity:0.1; }
   * }}}
  */
  def `link-opacity-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-10-hover:hover { --tblr-link-opacity:0.1; }
   * }}}
  */
  def `link-opacity-10-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-100 { --tblr-link-opacity:1; }
   * }}}
  */
  def `link-opacity-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-100-hover:hover { --tblr-link-opacity:1; }
   * }}}
  */
  def `link-opacity-100-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-25 { --tblr-link-opacity:0.25; }
   * }}}
  */
  def `link-opacity-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-25-hover:hover { --tblr-link-opacity:0.25; }
   * }}}
  */
  def `link-opacity-25-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-50 { --tblr-link-opacity:0.5; }
   * }}}
  */
  def `link-opacity-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-50-hover:hover { --tblr-link-opacity:0.5; }
   * }}}
  */
  def `link-opacity-50-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-75 { --tblr-link-opacity:0.75; }
   * }}}
  */
  def `link-opacity-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-opacity-75-hover:hover { --tblr-link-opacity:0.75; }
   * }}}
  */
  def `link-opacity-75-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-orange {
   *   color:RGBA(var(--tblr-orange-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-orange-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-orange-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-pink {
   *   color:RGBA(var(--tblr-pink-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-pink-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-pink-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-pinterest {
   *   color:RGBA(var(--tblr-pinterest-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-pinterest-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-pinterest-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-primary {
   *   color:RGBA(var(--tblr-primary-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-primary-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-primary-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-purple {
   *   color:RGBA(var(--tblr-purple-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-purple-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-purple-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-red {
   *   color:RGBA(var(--tblr-red-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-red-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-red-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-rss {
   *   color:RGBA(var(--tblr-rss-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-rss-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-rss-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-secondary {
   *   color:RGBA(var(--tblr-secondary-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-secondary-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-secondary-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-success {
   *   color:RGBA(var(--tblr-success-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-success-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-success-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-tabler {
   *   color:RGBA(var(--tblr-tabler-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-tabler-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-tabler-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-teal {
   *   color:RGBA(var(--tblr-teal-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-teal-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-teal-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-twitter {
   *   color:RGBA(var(--tblr-twitter-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-twitter-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-twitter-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-link-color-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:rgba(var(--tblr-link-color-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-underline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-azure {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-azure-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-azure-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-bitbucket {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-blue {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-blue-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-blue-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-cyan {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-cyan-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-cyan-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-danger {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-danger-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-danger-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-dark {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-dark-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-dark-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-dribbble {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-dribbble-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-dribbble-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-facebook {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-facebook-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-facebook-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-flickr {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-flickr-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-flickr-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-github {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-github-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-github-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-google {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-google-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-google-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-green {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-green-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-green-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-indigo {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-indigo-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-indigo-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-info {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-info-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-info-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-instagram {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-instagram-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-instagram-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-light {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-light-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-light-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-lime {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-lime-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-lime-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-linkedin {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-linkedin-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-linkedin-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-muted {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-muted-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-muted-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-0 { --tblr-link-underline-opacity:0; }
   * }}}
  */
  def `link-underline-opacity-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-0-hover:hover { --tblr-link-underline-opacity:0; }
   * }}}
  */
  def `link-underline-opacity-0-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-10 { --tblr-link-underline-opacity:0.1; }
   * }}}
  */
  def `link-underline-opacity-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-10-hover:hover { --tblr-link-underline-opacity:0.1; }
   * }}}
  */
  def `link-underline-opacity-10-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-100 { --tblr-link-underline-opacity:1; }
   * }}}
  */
  def `link-underline-opacity-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-100-hover:hover { --tblr-link-underline-opacity:1; }
   * }}}
  */
  def `link-underline-opacity-100-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-25 { --tblr-link-underline-opacity:0.25; }
   * }}}
  */
  def `link-underline-opacity-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-25-hover:hover { --tblr-link-underline-opacity:0.25; }
   * }}}
  */
  def `link-underline-opacity-25-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-50 { --tblr-link-underline-opacity:0.5; }
   * }}}
  */
  def `link-underline-opacity-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-50-hover:hover { --tblr-link-underline-opacity:0.5; }
   * }}}
  */
  def `link-underline-opacity-50-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-75 { --tblr-link-underline-opacity:0.75; }
   * }}}
  */
  def `link-underline-opacity-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-opacity-75-hover:hover { --tblr-link-underline-opacity:0.75; }
   * }}}
  */
  def `link-underline-opacity-75-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-orange {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-orange-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-orange-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-pink {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-pink-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-pink-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-pinterest {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-pinterest-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-pinterest-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-primary {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-primary-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-primary-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-purple {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-purple-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-purple-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-red {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-red-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-red-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-rss {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-rss-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-rss-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-secondary {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-secondary-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-secondary-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-success {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-success-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-success-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-tabler {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-tabler-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-tabler-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-teal {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-teal-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-teal-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-twitter {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-twitter-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-twitter-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-vimeo {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-vimeo-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-vimeo-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-vk {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-vk-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-vk-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-warning {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-warning-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-warning-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-yellow {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-yellow-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-yellow-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-underline-youtube {
   *   --tblr-link-underline-opacity:1;
   *   -webkit-text-decoration-color:rgba(var(--tblr-youtube-rgb),var(--tblr-link-underline-opacity)) !important;
   *   text-decoration-color:rgba(var(--tblr-youtube-rgb),var(--tblr-link-underline-opacity)) !important;
   * }
   * }}}
  */
  def `link-underline-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-vimeo {
   *   color:RGBA(var(--tblr-vimeo-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-vimeo-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-vimeo-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-vk {
   *   color:RGBA(var(--tblr-vk-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-vk-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-vk-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-warning {
   *   color:RGBA(var(--tblr-warning-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-warning-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-warning-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-yellow {
   *   color:RGBA(var(--tblr-yellow-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-yellow-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-yellow-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .link-youtube {
   *   color:RGBA(var(--tblr-youtube-rgb),var(--tblr-link-opacity,1)) !important;
   *   -webkit-text-decoration-color:RGBA(var(--tblr-youtube-rgb),var(--tblr-link-underline-opacity,1)) !important;
   *   text-decoration-color:RGBA(var(--tblr-youtube-rgb),var(--tblr-link-underline-opacity,1)) !important;
   * }
   * }}}
  */
  def `link-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-bordered .list-item {
   *   border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   margin-top:-1px;
   * }
   * }}}
  */
  def `list-bordered`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card>.list-group {
   *   border-top:inherit;
   *   border-bottom:inherit;
   * }
   * }}}
  */
  def `list-group`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-flush { border-radius:0; }
   * }}}
  */
  def `list-group-flush`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-header {
   *   background:var(--tblr-bg-surface-tertiary);
   *   padding:.5rem 1.25rem;
   *   font-size:.75rem;
   *   font-weight:var(--tblr-font-weight-medium);
   *   line-height:1;
   *   text-transform:uppercase;
   *   color:var(--tblr-secondary);
   *   border-bottom:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   * }
   * }}}
  */
  def `list-group-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-horizontal { flex-direction:row; }
   * }}}
  */
  def `list-group-horizontal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-horizontal-lg>.list-group-item+.list-group-item.active {
   *   margin-left:calc(-1*var(--tblr-list-group-border-width));
   *   border-left-width:var(--tblr-list-group-border-width);
   * }
   * }}}
  */
  def `list-group-horizontal-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-horizontal-md>.list-group-item+.list-group-item.active {
   *   margin-left:calc(-1*var(--tblr-list-group-border-width));
   *   border-left-width:var(--tblr-list-group-border-width);
   * }
   * }}}
  */
  def `list-group-horizontal-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-horizontal-sm>.list-group-item+.list-group-item.active {
   *   margin-left:calc(-1*var(--tblr-list-group-border-width));
   *   border-left-width:var(--tblr-list-group-border-width);
   * }
   * }}}
  */
  def `list-group-horizontal-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-horizontal-xl>.list-group-item+.list-group-item.active {
   *   margin-left:calc(-1*var(--tblr-list-group-border-width));
   *   border-left-width:var(--tblr-list-group-border-width);
   * }
   * }}}
  */
  def `list-group-horizontal-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-horizontal-xxl>.list-group-item+.list-group-item.active {
   *   margin-left:calc(-1*var(--tblr-list-group-border-width));
   *   border-left-width:var(--tblr-list-group-border-width);
   * }
   * }}}
  */
  def `list-group-horizontal-xxl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-hoverable .list-group-item-actions {
   *   opacity:0;
   *   transition:opacity .3s;
   * }
   * }}}
  */
  def `list-group-hoverable`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-numbered>.list-group-item::before {
   *   content:counters(section,".") ". ";
   *   counter-increment:section;
   * }
   * }}}
  */
  def `list-group-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-action {
   *   width:100%;
   *   color:var(--tblr-list-group-action-color);
   *   text-align:inherit;
   * }
   * }}}
  */
  def `list-group-item-action`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-hoverable .list-group-item-actions {
   *   opacity:0;
   *   transition:opacity .3s;
   * }
   * }}}
  */
  def `list-group-item-actions`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-azure {
   *   --tblr-list-group-color:var(--tblr-azure-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-azure-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-azure-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-azure-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-azure-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-azure-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-azure-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-azure-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-bitbucket {
   *   --tblr-list-group-color:var(--tblr-bitbucket-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-bitbucket-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-bitbucket-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-bitbucket-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-bitbucket-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-bitbucket-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-bitbucket-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-bitbucket-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-blue {
   *   --tblr-list-group-color:var(--tblr-blue-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-blue-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-blue-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-blue-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-blue-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-blue-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-blue-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-blue-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-cyan {
   *   --tblr-list-group-color:var(--tblr-cyan-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-cyan-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-cyan-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-cyan-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-cyan-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-cyan-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-cyan-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-cyan-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-danger {
   *   --tblr-list-group-color:var(--tblr-danger-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-danger-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-danger-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-danger-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-danger-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-danger-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-danger-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-danger-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-dark {
   *   --tblr-list-group-color:var(--tblr-dark-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-dark-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-dark-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-dark-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-dark-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-dark-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-dark-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-dark-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-dribbble {
   *   --tblr-list-group-color:var(--tblr-dribbble-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-dribbble-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-dribbble-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-dribbble-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-dribbble-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-dribbble-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-dribbble-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-dribbble-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-facebook {
   *   --tblr-list-group-color:var(--tblr-facebook-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-facebook-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-facebook-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-facebook-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-facebook-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-facebook-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-facebook-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-facebook-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-flickr {
   *   --tblr-list-group-color:var(--tblr-flickr-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-flickr-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-flickr-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-flickr-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-flickr-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-flickr-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-flickr-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-flickr-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-github {
   *   --tblr-list-group-color:var(--tblr-github-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-github-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-github-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-github-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-github-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-github-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-github-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-github-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-google {
   *   --tblr-list-group-color:var(--tblr-google-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-google-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-google-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-google-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-google-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-google-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-google-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-google-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-green {
   *   --tblr-list-group-color:var(--tblr-green-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-green-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-green-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-green-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-green-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-green-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-green-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-green-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-indigo {
   *   --tblr-list-group-color:var(--tblr-indigo-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-indigo-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-indigo-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-indigo-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-indigo-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-indigo-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-indigo-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-indigo-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-info {
   *   --tblr-list-group-color:var(--tblr-info-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-info-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-info-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-info-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-info-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-info-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-info-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-info-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-instagram {
   *   --tblr-list-group-color:var(--tblr-instagram-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-instagram-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-instagram-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-instagram-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-instagram-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-instagram-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-instagram-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-instagram-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-light {
   *   --tblr-list-group-color:var(--tblr-light-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-light-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-light-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-light-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-light-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-light-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-light-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-light-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-lime {
   *   --tblr-list-group-color:var(--tblr-lime-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-lime-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-lime-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-lime-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-lime-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-lime-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-lime-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-lime-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-linkedin {
   *   --tblr-list-group-color:var(--tblr-linkedin-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-linkedin-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-linkedin-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-linkedin-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-linkedin-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-linkedin-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-linkedin-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-linkedin-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-muted {
   *   --tblr-list-group-color:var(--tblr-muted-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-muted-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-muted-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-muted-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-muted-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-muted-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-muted-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-muted-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-orange {
   *   --tblr-list-group-color:var(--tblr-orange-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-orange-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-orange-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-orange-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-orange-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-orange-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-orange-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-orange-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-pink {
   *   --tblr-list-group-color:var(--tblr-pink-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-pink-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-pink-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-pink-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-pink-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-pink-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-pink-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-pink-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-pinterest {
   *   --tblr-list-group-color:var(--tblr-pinterest-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-pinterest-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-pinterest-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-pinterest-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-pinterest-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-pinterest-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-pinterest-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-pinterest-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-primary {
   *   --tblr-list-group-color:var(--tblr-primary-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-primary-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-primary-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-primary-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-primary-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-primary-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-primary-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-primary-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-purple {
   *   --tblr-list-group-color:var(--tblr-purple-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-purple-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-purple-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-purple-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-purple-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-purple-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-purple-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-purple-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-red {
   *   --tblr-list-group-color:var(--tblr-red-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-red-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-red-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-red-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-red-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-red-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-red-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-red-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-rss {
   *   --tblr-list-group-color:var(--tblr-rss-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-rss-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-rss-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-rss-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-rss-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-rss-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-rss-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-rss-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-secondary {
   *   --tblr-list-group-color:var(--tblr-secondary-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-secondary-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-secondary-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-secondary-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-secondary-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-secondary-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-secondary-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-secondary-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-success {
   *   --tblr-list-group-color:var(--tblr-success-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-success-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-success-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-success-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-success-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-success-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-success-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-success-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-tabler {
   *   --tblr-list-group-color:var(--tblr-tabler-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-tabler-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-tabler-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-tabler-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-tabler-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-tabler-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-tabler-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-tabler-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-teal {
   *   --tblr-list-group-color:var(--tblr-teal-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-teal-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-teal-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-teal-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-teal-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-teal-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-teal-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-teal-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-twitter {
   *   --tblr-list-group-color:var(--tblr-twitter-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-twitter-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-twitter-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-twitter-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-twitter-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-twitter-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-twitter-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-twitter-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-vimeo {
   *   --tblr-list-group-color:var(--tblr-vimeo-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-vimeo-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-vimeo-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-vimeo-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-vimeo-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-vimeo-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-vimeo-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-vimeo-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-vk {
   *   --tblr-list-group-color:var(--tblr-vk-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-vk-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-vk-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-vk-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-vk-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-vk-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-vk-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-vk-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-warning {
   *   --tblr-list-group-color:var(--tblr-warning-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-warning-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-warning-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-warning-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-warning-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-warning-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-warning-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-warning-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-yellow {
   *   --tblr-list-group-color:var(--tblr-yellow-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-yellow-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-yellow-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-yellow-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-yellow-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-yellow-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-yellow-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-yellow-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-item-youtube {
   *   --tblr-list-group-color:var(--tblr-youtube-text-emphasis);
   *   --tblr-list-group-bg:var(--tblr-youtube-bg-subtle);
   *   --tblr-list-group-border-color:var(--tblr-youtube-border-subtle);
   *   --tblr-list-group-action-hover-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-hover-bg:var(--tblr-youtube-border-subtle);
   *   --tblr-list-group-action-active-color:var(--tblr-emphasis-color);
   *   --tblr-list-group-action-active-bg:var(--tblr-youtube-border-subtle);
   *   --tblr-list-group-active-color:var(--tblr-youtube-bg-subtle);
   *   --tblr-list-group-active-bg:var(--tblr-youtube-text-emphasis);
   *   --tblr-list-group-active-border-color:var(--tblr-youtube-text-emphasis);
   * }
   * }}}
  */
  def `list-group-item-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-numbered {
   *   list-style-type:none;
   *   counter-reset:section;
   * }
   * }}}
  */
  def `list-group-numbered`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-group-transparent {
   *   --tblr-list-group-border-radius:0;
   *   margin:0 -1.25rem;
   * }
   * }}}
  */
  def `list-group-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-inline {
   *   padding-left:0;
   *   list-style:none;
   * }
   * }}}
  */
  def `list-inline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-inline-dots .list-inline-item+.list-inline-item:before {
   *   content:" · ";
   *   -webkit-margin-end:.5rem;
   *   margin-inline-end:.5rem;
   * }
   * }}}
  */
  def `list-inline-dots`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-inline-item { display:inline-block; }
   * }}}
  */
  def `list-inline-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-bordered .list-item {
   *   border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   margin-top:-1px;
   * }
   * }}}
  */
  def `list-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-separated-item { padding:1rem 0; }
   * }}}
  */
  def `list-separated-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .list-unstyled {
   *   padding-left:0;
   *   list-style:none;
   * }
   * }}}
  */
  def `list-unstyled`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker {
   *   --litepicker-month-weekday-color:var(--tblr-secondary);
   *   --litepicker-button-prev-month-color:var(--tblr-secondary);
   *   --litepicker-button-next-month-color:var(--tblr-secondary);
   *   --litepicker-button-prev-month-color-hover:var(--tblr-primary);
   *   --litepicker-button-next-month-color-hover:var(--tblr-primary);
   *   --litepicker-day-color:var(--tblr-body-color);
   *   --litepicker-day-color-hover:var(--tblr-primary);
   *   --litepicker-is-end-color-bg:var(--tblr-primary);
   *   --litepicker-is-today-color:var(--tblr-primary);
   *   --litepicker-month-header-color:var(--tblr-body-color);
   *   --litepicker-container-months-color-bg:var(--tblr-bg-surface);
   *   font:inherit;
   *   -webkit-user-select:none;
   *   -moz-user-select:none;
   *   -ms-user-select:none;
   *   user-select:none;
   * }
   * }}}
  */
  def litepicker: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .loader {
   *   position:relative;
   *   display:block;
   *   width:2.5rem;
   *   height:2.5rem;
   *   color:#0054a6;
   *   vertical-align:middle;
   * }
   * }}}
  */
  def loader: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-0 { margin:0 !important; }
   * }}}
  */
  def `m-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-1 { margin:.25rem !important; }
   * }}}
  */
  def `m-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-2 { margin:.5rem !important; }
   * }}}
  */
  def `m-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-3 { margin:1rem !important; }
   * }}}
  */
  def `m-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-4 { margin:1.5rem !important; }
   * }}}
  */
  def `m-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-5 { margin:2rem !important; }
   * }}}
  */
  def `m-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-6 { margin:3rem !important; }
   * }}}
  */
  def `m-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-7 { margin:5rem !important; }
   * }}}
  */
  def `m-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-8 { margin:8rem !important; }
   * }}}
  */
  def `m-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-auto { margin:auto !important; }
   * }}}
  */
  def `m-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-0 { margin:0 !important; }
   * }}}
  */
  def `m-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-1 { margin:.25rem !important; }
   * }}}
  */
  def `m-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-2 { margin:.5rem !important; }
   * }}}
  */
  def `m-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-3 { margin:1rem !important; }
   * }}}
  */
  def `m-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-4 { margin:1.5rem !important; }
   * }}}
  */
  def `m-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-5 { margin:2rem !important; }
   * }}}
  */
  def `m-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-6 { margin:3rem !important; }
   * }}}
  */
  def `m-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-7 { margin:5rem !important; }
   * }}}
  */
  def `m-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-8 { margin:8rem !important; }
   * }}}
  */
  def `m-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-lg-auto { margin:auto !important; }
   * }}}
  */
  def `m-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-0 { margin:0 !important; }
   * }}}
  */
  def `m-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-1 { margin:.25rem !important; }
   * }}}
  */
  def `m-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-2 { margin:.5rem !important; }
   * }}}
  */
  def `m-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-3 { margin:1rem !important; }
   * }}}
  */
  def `m-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-4 { margin:1.5rem !important; }
   * }}}
  */
  def `m-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-5 { margin:2rem !important; }
   * }}}
  */
  def `m-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-6 { margin:3rem !important; }
   * }}}
  */
  def `m-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-7 { margin:5rem !important; }
   * }}}
  */
  def `m-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-8 { margin:8rem !important; }
   * }}}
  */
  def `m-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-md-auto { margin:auto !important; }
   * }}}
  */
  def `m-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-0 { margin:0 !important; }
   * }}}
  */
  def `m-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-1 { margin:.25rem !important; }
   * }}}
  */
  def `m-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-2 { margin:.5rem !important; }
   * }}}
  */
  def `m-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-3 { margin:1rem !important; }
   * }}}
  */
  def `m-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-4 { margin:1.5rem !important; }
   * }}}
  */
  def `m-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-5 { margin:2rem !important; }
   * }}}
  */
  def `m-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-6 { margin:3rem !important; }
   * }}}
  */
  def `m-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-7 { margin:5rem !important; }
   * }}}
  */
  def `m-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-8 { margin:8rem !important; }
   * }}}
  */
  def `m-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-sm-auto { margin:auto !important; }
   * }}}
  */
  def `m-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-0 { margin:0 !important; }
   * }}}
  */
  def `m-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-1 { margin:.25rem !important; }
   * }}}
  */
  def `m-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-2 { margin:.5rem !important; }
   * }}}
  */
  def `m-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-3 { margin:1rem !important; }
   * }}}
  */
  def `m-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-4 { margin:1.5rem !important; }
   * }}}
  */
  def `m-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-5 { margin:2rem !important; }
   * }}}
  */
  def `m-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-6 { margin:3rem !important; }
   * }}}
  */
  def `m-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-7 { margin:5rem !important; }
   * }}}
  */
  def `m-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-8 { margin:8rem !important; }
   * }}}
  */
  def `m-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xl-auto { margin:auto !important; }
   * }}}
  */
  def `m-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-0 { margin:0 !important; }
   * }}}
  */
  def `m-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-1 { margin:.25rem !important; }
   * }}}
  */
  def `m-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-2 { margin:.5rem !important; }
   * }}}
  */
  def `m-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-3 { margin:1rem !important; }
   * }}}
  */
  def `m-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-4 { margin:1.5rem !important; }
   * }}}
  */
  def `m-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-5 { margin:2rem !important; }
   * }}}
  */
  def `m-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-6 { margin:3rem !important; }
   * }}}
  */
  def `m-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-7 { margin:5rem !important; }
   * }}}
  */
  def `m-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-8 { margin:8rem !important; }
   * }}}
  */
  def `m-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .m-xxl-auto { margin:auto !important; }
   * }}}
  */
  def `m-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mark,
   * mark {
   *   padding:.1875em;
   *   background-color:var(--tblr-highlight-bg);
   * }
   * }}}
  */
  def mark: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .markdown>table,
   * .table {
   *   --tblr-table-color-type:initial;
   *   --tblr-table-bg-type:initial;
   *   --tblr-table-color-state:initial;
   *   --tblr-table-bg-state:initial;
   *   --tblr-table-color:inherit;
   *   --tblr-table-bg:transparent;
   *   --tblr-table-border-color:var(--tblr-border-color-translucent);
   *   --tblr-table-accent-bg:transparent;
   *   --tblr-table-striped-color:inherit;
   *   --tblr-table-striped-bg:var(--tblr-bg-surface-tertiary);
   *   --tblr-table-active-color:inherit;
   *   --tblr-table-active-bg:rgba(0,0,0,0.1);
   *   --tblr-table-hover-color:inherit;
   *   --tblr-table-hover-bg:rgba(0,0,0,0.075);
   *   width:100%;
   *   margin-bottom:1rem;
   *   vertical-align:top;
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def markdown: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-0 { margin-bottom:0 !important; }
   * }}}
  */
  def `mb-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-1 { margin-bottom:.25rem !important; }
   * }}}
  */
  def `mb-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-2 { margin-bottom:.5rem !important; }
   * }}}
  */
  def `mb-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-3 { margin-bottom:1rem !important; }
   * }}}
  */
  def `mb-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-4 { margin-bottom:1.5rem !important; }
   * }}}
  */
  def `mb-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-5 { margin-bottom:2rem !important; }
   * }}}
  */
  def `mb-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-6 { margin-bottom:3rem !important; }
   * }}}
  */
  def `mb-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-7 { margin-bottom:5rem !important; }
   * }}}
  */
  def `mb-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-8 { margin-bottom:8rem !important; }
   * }}}
  */
  def `mb-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-auto { margin-bottom:auto !important; }
   * }}}
  */
  def `mb-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-0 { margin-bottom:0 !important; }
   * }}}
  */
  def `mb-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-1 { margin-bottom:.25rem !important; }
   * }}}
  */
  def `mb-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-2 { margin-bottom:.5rem !important; }
   * }}}
  */
  def `mb-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-3 { margin-bottom:1rem !important; }
   * }}}
  */
  def `mb-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-4 { margin-bottom:1.5rem !important; }
   * }}}
  */
  def `mb-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-5 { margin-bottom:2rem !important; }
   * }}}
  */
  def `mb-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-6 { margin-bottom:3rem !important; }
   * }}}
  */
  def `mb-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-7 { margin-bottom:5rem !important; }
   * }}}
  */
  def `mb-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-8 { margin-bottom:8rem !important; }
   * }}}
  */
  def `mb-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-lg-auto { margin-bottom:auto !important; }
   * }}}
  */
  def `mb-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-0 { margin-bottom:0 !important; }
   * }}}
  */
  def `mb-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-1 { margin-bottom:.25rem !important; }
   * }}}
  */
  def `mb-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-2 { margin-bottom:.5rem !important; }
   * }}}
  */
  def `mb-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-3 { margin-bottom:1rem !important; }
   * }}}
  */
  def `mb-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-4 { margin-bottom:1.5rem !important; }
   * }}}
  */
  def `mb-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-5 { margin-bottom:2rem !important; }
   * }}}
  */
  def `mb-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-6 { margin-bottom:3rem !important; }
   * }}}
  */
  def `mb-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-7 { margin-bottom:5rem !important; }
   * }}}
  */
  def `mb-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-8 { margin-bottom:8rem !important; }
   * }}}
  */
  def `mb-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-md-auto { margin-bottom:auto !important; }
   * }}}
  */
  def `mb-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-0 { margin-bottom:0 !important; }
   * }}}
  */
  def `mb-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-1 { margin-bottom:.25rem !important; }
   * }}}
  */
  def `mb-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-2 { margin-bottom:.5rem !important; }
   * }}}
  */
  def `mb-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-3 { margin-bottom:1rem !important; }
   * }}}
  */
  def `mb-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-4 { margin-bottom:1.5rem !important; }
   * }}}
  */
  def `mb-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-5 { margin-bottom:2rem !important; }
   * }}}
  */
  def `mb-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-6 { margin-bottom:3rem !important; }
   * }}}
  */
  def `mb-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-7 { margin-bottom:5rem !important; }
   * }}}
  */
  def `mb-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-8 { margin-bottom:8rem !important; }
   * }}}
  */
  def `mb-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-sm-auto { margin-bottom:auto !important; }
   * }}}
  */
  def `mb-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-0 { margin-bottom:0 !important; }
   * }}}
  */
  def `mb-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-1 { margin-bottom:.25rem !important; }
   * }}}
  */
  def `mb-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-2 { margin-bottom:.5rem !important; }
   * }}}
  */
  def `mb-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-3 { margin-bottom:1rem !important; }
   * }}}
  */
  def `mb-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-4 { margin-bottom:1.5rem !important; }
   * }}}
  */
  def `mb-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-5 { margin-bottom:2rem !important; }
   * }}}
  */
  def `mb-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-6 { margin-bottom:3rem !important; }
   * }}}
  */
  def `mb-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-7 { margin-bottom:5rem !important; }
   * }}}
  */
  def `mb-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-8 { margin-bottom:8rem !important; }
   * }}}
  */
  def `mb-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xl-auto { margin-bottom:auto !important; }
   * }}}
  */
  def `mb-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-0 { margin-bottom:0 !important; }
   * }}}
  */
  def `mb-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-1 { margin-bottom:.25rem !important; }
   * }}}
  */
  def `mb-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-2 { margin-bottom:.5rem !important; }
   * }}}
  */
  def `mb-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-3 { margin-bottom:1rem !important; }
   * }}}
  */
  def `mb-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-4 { margin-bottom:1.5rem !important; }
   * }}}
  */
  def `mb-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-5 { margin-bottom:2rem !important; }
   * }}}
  */
  def `mb-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-6 { margin-bottom:3rem !important; }
   * }}}
  */
  def `mb-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-7 { margin-bottom:5rem !important; }
   * }}}
  */
  def `mb-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-8 { margin-bottom:8rem !important; }
   * }}}
  */
  def `mb-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mb-xxl-auto { margin-bottom:auto !important; }
   * }}}
  */
  def `mb-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-0 { margin-right:0 !important; }
   * }}}
  */
  def `me-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-1 { margin-right:.25rem !important; }
   * }}}
  */
  def `me-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-2 { margin-right:.5rem !important; }
   * }}}
  */
  def `me-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-3 { margin-right:1rem !important; }
   * }}}
  */
  def `me-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-4 { margin-right:1.5rem !important; }
   * }}}
  */
  def `me-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-5 { margin-right:2rem !important; }
   * }}}
  */
  def `me-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-6 { margin-right:3rem !important; }
   * }}}
  */
  def `me-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-7 { margin-right:5rem !important; }
   * }}}
  */
  def `me-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-8 { margin-right:8rem !important; }
   * }}}
  */
  def `me-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-auto { margin-right:auto !important; }
   * }}}
  */
  def `me-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-0 { margin-right:0 !important; }
   * }}}
  */
  def `me-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-1 { margin-right:.25rem !important; }
   * }}}
  */
  def `me-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-2 { margin-right:.5rem !important; }
   * }}}
  */
  def `me-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-3 { margin-right:1rem !important; }
   * }}}
  */
  def `me-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-4 { margin-right:1.5rem !important; }
   * }}}
  */
  def `me-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-5 { margin-right:2rem !important; }
   * }}}
  */
  def `me-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-6 { margin-right:3rem !important; }
   * }}}
  */
  def `me-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-7 { margin-right:5rem !important; }
   * }}}
  */
  def `me-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-8 { margin-right:8rem !important; }
   * }}}
  */
  def `me-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-lg-auto { margin-right:auto !important; }
   * }}}
  */
  def `me-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-0 { margin-right:0 !important; }
   * }}}
  */
  def `me-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-1 { margin-right:.25rem !important; }
   * }}}
  */
  def `me-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-2 { margin-right:.5rem !important; }
   * }}}
  */
  def `me-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-3 { margin-right:1rem !important; }
   * }}}
  */
  def `me-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-4 { margin-right:1.5rem !important; }
   * }}}
  */
  def `me-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-5 { margin-right:2rem !important; }
   * }}}
  */
  def `me-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-6 { margin-right:3rem !important; }
   * }}}
  */
  def `me-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-7 { margin-right:5rem !important; }
   * }}}
  */
  def `me-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-8 { margin-right:8rem !important; }
   * }}}
  */
  def `me-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-md-auto { margin-right:auto !important; }
   * }}}
  */
  def `me-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-0 { margin-right:0 !important; }
   * }}}
  */
  def `me-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-1 { margin-right:.25rem !important; }
   * }}}
  */
  def `me-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-2 { margin-right:.5rem !important; }
   * }}}
  */
  def `me-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-3 { margin-right:1rem !important; }
   * }}}
  */
  def `me-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-4 { margin-right:1.5rem !important; }
   * }}}
  */
  def `me-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-5 { margin-right:2rem !important; }
   * }}}
  */
  def `me-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-6 { margin-right:3rem !important; }
   * }}}
  */
  def `me-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-7 { margin-right:5rem !important; }
   * }}}
  */
  def `me-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-8 { margin-right:8rem !important; }
   * }}}
  */
  def `me-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-sm-auto { margin-right:auto !important; }
   * }}}
  */
  def `me-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-0 { margin-right:0 !important; }
   * }}}
  */
  def `me-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-1 { margin-right:.25rem !important; }
   * }}}
  */
  def `me-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-2 { margin-right:.5rem !important; }
   * }}}
  */
  def `me-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-3 { margin-right:1rem !important; }
   * }}}
  */
  def `me-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-4 { margin-right:1.5rem !important; }
   * }}}
  */
  def `me-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-5 { margin-right:2rem !important; }
   * }}}
  */
  def `me-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-6 { margin-right:3rem !important; }
   * }}}
  */
  def `me-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-7 { margin-right:5rem !important; }
   * }}}
  */
  def `me-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-8 { margin-right:8rem !important; }
   * }}}
  */
  def `me-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xl-auto { margin-right:auto !important; }
   * }}}
  */
  def `me-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-0 { margin-right:0 !important; }
   * }}}
  */
  def `me-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-1 { margin-right:.25rem !important; }
   * }}}
  */
  def `me-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-2 { margin-right:.5rem !important; }
   * }}}
  */
  def `me-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-3 { margin-right:1rem !important; }
   * }}}
  */
  def `me-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-4 { margin-right:1.5rem !important; }
   * }}}
  */
  def `me-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-5 { margin-right:2rem !important; }
   * }}}
  */
  def `me-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-6 { margin-right:3rem !important; }
   * }}}
  */
  def `me-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-7 { margin-right:5rem !important; }
   * }}}
  */
  def `me-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-8 { margin-right:8rem !important; }
   * }}}
  */
  def `me-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .me-xxl-auto { margin-right:auto !important; }
   * }}}
  */
  def `me-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mh-100 { max-height:100% !important; }
   * }}}
  */
  def `mh-100`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .mi,
   * .highlight .s1 { color:#d9a9ff; }
   * }}}
  */
  def mi: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .min-vh-100 { min-height:100vh !important; }
   * }}}
  */
  def `min-vh-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .min-vw-100 { min-width:100vw !important; }
   * }}}
  */
  def `min-vw-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal {
   *   --tblr-modal-zindex:1055;
   *   --tblr-modal-width:540px;
   *   --tblr-modal-padding:1.5rem;
   *   --tblr-modal-margin:0.5rem;
   *   --tblr-modal-bg:var(--tblr-bg-surface);
   *   --tblr-modal-border-color:transparent;
   *   --tblr-modal-border-width:var(--tblr-border-width);
   *   --tblr-modal-border-radius:var(--tblr-border-radius-lg);
   *   --tblr-modal-box-shadow:0 0.125rem 0.25rem rgba(0,0,0,0.075);
   *   --tblr-modal-inner-border-radius:calc(var(--tblr-modal-border-radius) - 1px);
   *   --tblr-modal-header-padding-x:1.5rem;
   *   --tblr-modal-header-padding-y:1.5rem;
   *   --tblr-modal-header-padding:1.5rem;
   *   --tblr-modal-header-border-color:var(--tblr-border-color);
   *   --tblr-modal-header-border-width:var(--tblr-border-width);
   *   --tblr-modal-title-line-height:1.4285714286;
   *   --tblr-modal-footer-gap:0.75rem;
   *   --tblr-modal-footer-bg:var(--tblr-bg-surface-tertiary);
   *   --tblr-modal-footer-border-color:var(--tblr-border-color);
   *   --tblr-modal-footer-border-width:var(--tblr-border-width);
   *   position:fixed;
   *   top:0;
   *   left:0;
   *   z-index:var(--tblr-modal-zindex);
   *   display:none;
   *   width:100%;
   *   height:100%;
   *   overflow-x:hidden;
   *   overflow-y:auto;
   *   outline:0;
   * }
   * }}}
  */
  def modal: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-backdrop {
   *   --tblr-backdrop-zindex:1050;
   *   --tblr-backdrop-bg:#182433;
   *   --tblr-backdrop-opacity:0.24;
   *   position:fixed;
   *   top:0;
   *   left:0;
   *   z-index:var(--tblr-backdrop-zindex);
   *   width:100vw;
   *   height:100vh;
   *   background-color:var(--tblr-backdrop-bg);
   * }
   * }}}
  */
  def `modal-backdrop`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-blur {
   *   -webkit-backdrop-filter:blur(4px);
   *   backdrop-filter:blur(4px);
   * }
   * }}}
  */
  def `modal-blur`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-dialog-scrollable .modal-body { overflow-y:auto; }
   * }}}
  */
  def `modal-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-dialog-scrollable .modal-content {
   *   max-height:100%;
   *   overflow:hidden;
   * }
   * }}}
  */
  def `modal-content`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-dialog {
   *   position:relative;
   *   width:auto;
   *   margin:var(--tblr-modal-margin);
   *   pointer-events:none;
   * }
   * }}}
  */
  def `modal-dialog`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-dialog-centered {
   *   display:flex;
   *   align-items:center;
   *   min-height:calc(100% - var(--tblr-modal-margin)*2);
   * }
   * }}}
  */
  def `modal-dialog-centered`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-dialog-scrollable { height:calc(100% - var(--tblr-modal-margin)*2); }
   * }}}
  */
  def `modal-dialog-scrollable`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-footer {
   *   display:flex;
   *   flex-shrink:0;
   *   flex-wrap:wrap;
   *   align-items:center;
   *   justify-content:flex-end;
   *   padding:calc(var(--tblr-modal-padding) - var(--tblr-modal-footer-gap)*.5);
   *   background-color:var(--tblr-modal-footer-bg);
   *   border-top:var(--tblr-modal-footer-border-width) solid var(--tblr-modal-footer-border-color);
   *   border-bottom-right-radius:var(--tblr-modal-inner-border-radius);
   *   border-bottom-left-radius:var(--tblr-modal-inner-border-radius);
   * }
   * }}}
  */
  def `modal-footer`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-full-width {
   *   max-width:none;
   *   margin:0 .5rem;
   * }
   * }}}
  */
  def `modal-full-width`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-fullscreen {
   *   width:100vw;
   *   max-width:none;
   *   height:100%;
   *   margin:0;
   * }
   * }}}
  */
  def `modal-fullscreen`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-fullscreen-lg-down .modal-body { overflow-y:auto; }
   * }}}
  */
  def `modal-fullscreen-lg-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-fullscreen-md-down .modal-body { overflow-y:auto; }
   * }}}
  */
  def `modal-fullscreen-md-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-fullscreen-sm-down .modal-body { overflow-y:auto; }
   * }}}
  */
  def `modal-fullscreen-sm-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-fullscreen-xl-down .modal-body { overflow-y:auto; }
   * }}}
  */
  def `modal-fullscreen-xl-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-fullscreen-xxl-down .modal-body { overflow-y:auto; }
   * }}}
  */
  def `modal-fullscreen-xxl-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-header {
   *   display:flex;
   *   flex-shrink:0;
   *   align-items:center;
   *   justify-content:space-between;
   *   padding:var(--tblr-modal-header-padding);
   *   border-bottom:var(--tblr-modal-header-border-width) solid var(--tblr-modal-header-border-color);
   *   border-top-left-radius:var(--tblr-modal-inner-border-radius);
   *   border-top-right-radius:var(--tblr-modal-inner-border-radius);
   * }
   * }}}
  */
  def `modal-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-lg,
   * .modal-xl { --tblr-modal-width:720px; }
   * }}}
  */
  def `modal-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-sm { --tblr-modal-width:380px; }
   * }}}
  */
  def `modal-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal.modal-static .modal-dialog { transform:scale(1.02); }
   * }}}
  */
  def `modal-static`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-status {
   *   position:absolute;
   *   top:0;
   *   left:0;
   *   right:0;
   *   height:2px;
   *   background:var(--tblr-secondary);
   *   border-radius:var(--tblr-border-radius-lg) var(--tblr-border-radius-lg) 0 0;
   * }
   * }}}
  */
  def `modal-status`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-title {
   *   margin-bottom:0;
   *   line-height:var(--tblr-modal-title-line-height);
   * }
   * }}}
  */
  def `modal-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .modal-lg,
   * .modal-xl { --tblr-modal-width:720px; }
   * }}}
  */
  def `modal-xl`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__months .month-item-name,
   * .litepicker .container__months .month-item-year { font-weight:var(--tblr-font-weight-medium) !important; }
   * }}}
  */
  def `month-item-name`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__months .month-item-weekdays-row>div {
   *   padding:.5rem 0 !important;
   *   font-size:.75rem;
   * }
   * }}}
  */
  def `month-item-weekdays-row`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .litepicker .container__months .month-item-name,
   * .litepicker .container__months .month-item-year { font-weight:var(--tblr-font-weight-medium) !important; }
   * }}}
  */
  def `month-item-year`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-0 { margin-left:0 !important; }
   * }}}
  */
  def `ms-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-1 { margin-left:.25rem !important; }
   * }}}
  */
  def `ms-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-2 { margin-left:.5rem !important; }
   * }}}
  */
  def `ms-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-3 { margin-left:1rem !important; }
   * }}}
  */
  def `ms-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-4 { margin-left:1.5rem !important; }
   * }}}
  */
  def `ms-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-5 { margin-left:2rem !important; }
   * }}}
  */
  def `ms-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-6 { margin-left:3rem !important; }
   * }}}
  */
  def `ms-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-7 { margin-left:5rem !important; }
   * }}}
  */
  def `ms-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-8 { margin-left:8rem !important; }
   * }}}
  */
  def `ms-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-auto { margin-left:auto !important; }
   * }}}
  */
  def `ms-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-0 { margin-left:0 !important; }
   * }}}
  */
  def `ms-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-1 { margin-left:.25rem !important; }
   * }}}
  */
  def `ms-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-2 { margin-left:.5rem !important; }
   * }}}
  */
  def `ms-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-3 { margin-left:1rem !important; }
   * }}}
  */
  def `ms-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-4 { margin-left:1.5rem !important; }
   * }}}
  */
  def `ms-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-5 { margin-left:2rem !important; }
   * }}}
  */
  def `ms-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-6 { margin-left:3rem !important; }
   * }}}
  */
  def `ms-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-7 { margin-left:5rem !important; }
   * }}}
  */
  def `ms-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-8 { margin-left:8rem !important; }
   * }}}
  */
  def `ms-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-lg-auto { margin-left:auto !important; }
   * }}}
  */
  def `ms-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-0 { margin-left:0 !important; }
   * }}}
  */
  def `ms-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-1 { margin-left:.25rem !important; }
   * }}}
  */
  def `ms-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-2 { margin-left:.5rem !important; }
   * }}}
  */
  def `ms-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-3 { margin-left:1rem !important; }
   * }}}
  */
  def `ms-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-4 { margin-left:1.5rem !important; }
   * }}}
  */
  def `ms-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-5 { margin-left:2rem !important; }
   * }}}
  */
  def `ms-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-6 { margin-left:3rem !important; }
   * }}}
  */
  def `ms-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-7 { margin-left:5rem !important; }
   * }}}
  */
  def `ms-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-8 { margin-left:8rem !important; }
   * }}}
  */
  def `ms-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-md-auto { margin-left:auto !important; }
   * }}}
  */
  def `ms-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-0 { margin-left:0 !important; }
   * }}}
  */
  def `ms-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-1 { margin-left:.25rem !important; }
   * }}}
  */
  def `ms-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-2 { margin-left:.5rem !important; }
   * }}}
  */
  def `ms-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-3 { margin-left:1rem !important; }
   * }}}
  */
  def `ms-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-4 { margin-left:1.5rem !important; }
   * }}}
  */
  def `ms-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-5 { margin-left:2rem !important; }
   * }}}
  */
  def `ms-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-6 { margin-left:3rem !important; }
   * }}}
  */
  def `ms-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-7 { margin-left:5rem !important; }
   * }}}
  */
  def `ms-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-8 { margin-left:8rem !important; }
   * }}}
  */
  def `ms-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-sm-auto { margin-left:auto !important; }
   * }}}
  */
  def `ms-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-0 { margin-left:0 !important; }
   * }}}
  */
  def `ms-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-1 { margin-left:.25rem !important; }
   * }}}
  */
  def `ms-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-2 { margin-left:.5rem !important; }
   * }}}
  */
  def `ms-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-3 { margin-left:1rem !important; }
   * }}}
  */
  def `ms-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-4 { margin-left:1.5rem !important; }
   * }}}
  */
  def `ms-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-5 { margin-left:2rem !important; }
   * }}}
  */
  def `ms-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-6 { margin-left:3rem !important; }
   * }}}
  */
  def `ms-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-7 { margin-left:5rem !important; }
   * }}}
  */
  def `ms-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-8 { margin-left:8rem !important; }
   * }}}
  */
  def `ms-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xl-auto { margin-left:auto !important; }
   * }}}
  */
  def `ms-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-0 { margin-left:0 !important; }
   * }}}
  */
  def `ms-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-1 { margin-left:.25rem !important; }
   * }}}
  */
  def `ms-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-2 { margin-left:.5rem !important; }
   * }}}
  */
  def `ms-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-3 { margin-left:1rem !important; }
   * }}}
  */
  def `ms-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-4 { margin-left:1.5rem !important; }
   * }}}
  */
  def `ms-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-5 { margin-left:2rem !important; }
   * }}}
  */
  def `ms-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-6 { margin-left:3rem !important; }
   * }}}
  */
  def `ms-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-7 { margin-left:5rem !important; }
   * }}}
  */
  def `ms-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-8 { margin-left:8rem !important; }
   * }}}
  */
  def `ms-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ms-xxl-auto { margin-left:auto !important; }
   * }}}
  */
  def `ms-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-0 { margin-top:0 !important; }
   * }}}
  */
  def `mt-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-1 { margin-top:.25rem !important; }
   * }}}
  */
  def `mt-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-2 { margin-top:.5rem !important; }
   * }}}
  */
  def `mt-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-3 { margin-top:1rem !important; }
   * }}}
  */
  def `mt-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-4 { margin-top:1.5rem !important; }
   * }}}
  */
  def `mt-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-5 { margin-top:2rem !important; }
   * }}}
  */
  def `mt-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-6 { margin-top:3rem !important; }
   * }}}
  */
  def `mt-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-7 { margin-top:5rem !important; }
   * }}}
  */
  def `mt-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-8 { margin-top:8rem !important; }
   * }}}
  */
  def `mt-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-auto { margin-top:auto !important; }
   * }}}
  */
  def `mt-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-0 { margin-top:0 !important; }
   * }}}
  */
  def `mt-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-1 { margin-top:.25rem !important; }
   * }}}
  */
  def `mt-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-2 { margin-top:.5rem !important; }
   * }}}
  */
  def `mt-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-3 { margin-top:1rem !important; }
   * }}}
  */
  def `mt-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-4 { margin-top:1.5rem !important; }
   * }}}
  */
  def `mt-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-5 { margin-top:2rem !important; }
   * }}}
  */
  def `mt-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-6 { margin-top:3rem !important; }
   * }}}
  */
  def `mt-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-7 { margin-top:5rem !important; }
   * }}}
  */
  def `mt-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-8 { margin-top:8rem !important; }
   * }}}
  */
  def `mt-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-lg-auto { margin-top:auto !important; }
   * }}}
  */
  def `mt-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-0 { margin-top:0 !important; }
   * }}}
  */
  def `mt-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-1 { margin-top:.25rem !important; }
   * }}}
  */
  def `mt-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-2 { margin-top:.5rem !important; }
   * }}}
  */
  def `mt-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-3 { margin-top:1rem !important; }
   * }}}
  */
  def `mt-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-4 { margin-top:1.5rem !important; }
   * }}}
  */
  def `mt-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-5 { margin-top:2rem !important; }
   * }}}
  */
  def `mt-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-6 { margin-top:3rem !important; }
   * }}}
  */
  def `mt-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-7 { margin-top:5rem !important; }
   * }}}
  */
  def `mt-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-8 { margin-top:8rem !important; }
   * }}}
  */
  def `mt-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-md-auto { margin-top:auto !important; }
   * }}}
  */
  def `mt-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-0 { margin-top:0 !important; }
   * }}}
  */
  def `mt-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-1 { margin-top:.25rem !important; }
   * }}}
  */
  def `mt-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-2 { margin-top:.5rem !important; }
   * }}}
  */
  def `mt-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-3 { margin-top:1rem !important; }
   * }}}
  */
  def `mt-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-4 { margin-top:1.5rem !important; }
   * }}}
  */
  def `mt-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-5 { margin-top:2rem !important; }
   * }}}
  */
  def `mt-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-6 { margin-top:3rem !important; }
   * }}}
  */
  def `mt-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-7 { margin-top:5rem !important; }
   * }}}
  */
  def `mt-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-8 { margin-top:8rem !important; }
   * }}}
  */
  def `mt-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-sm-auto { margin-top:auto !important; }
   * }}}
  */
  def `mt-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-0 { margin-top:0 !important; }
   * }}}
  */
  def `mt-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-1 { margin-top:.25rem !important; }
   * }}}
  */
  def `mt-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-2 { margin-top:.5rem !important; }
   * }}}
  */
  def `mt-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-3 { margin-top:1rem !important; }
   * }}}
  */
  def `mt-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-4 { margin-top:1.5rem !important; }
   * }}}
  */
  def `mt-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-5 { margin-top:2rem !important; }
   * }}}
  */
  def `mt-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-6 { margin-top:3rem !important; }
   * }}}
  */
  def `mt-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-7 { margin-top:5rem !important; }
   * }}}
  */
  def `mt-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-8 { margin-top:8rem !important; }
   * }}}
  */
  def `mt-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xl-auto { margin-top:auto !important; }
   * }}}
  */
  def `mt-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-0 { margin-top:0 !important; }
   * }}}
  */
  def `mt-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-1 { margin-top:.25rem !important; }
   * }}}
  */
  def `mt-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-2 { margin-top:.5rem !important; }
   * }}}
  */
  def `mt-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-3 { margin-top:1rem !important; }
   * }}}
  */
  def `mt-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-4 { margin-top:1.5rem !important; }
   * }}}
  */
  def `mt-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-5 { margin-top:2rem !important; }
   * }}}
  */
  def `mt-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-6 { margin-top:3rem !important; }
   * }}}
  */
  def `mt-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-7 { margin-top:5rem !important; }
   * }}}
  */
  def `mt-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-8 { margin-top:8rem !important; }
   * }}}
  */
  def `mt-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mt-xxl-auto { margin-top:auto !important; }
   * }}}
  */
  def `mt-xxl-auto`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-drag_drop.multi>.ts-control>div.ui-sortable-placeholder {
   *   visibility:visible !important;
   *   background:#f2f2f2 !important;
   *   background:rgba(0,0,0,.06) !important;
   *   border:0 none !important;
   *   box-shadow:inset 0 0 12px 4px #fff;
   * }
   * }}}
  */
  def multi: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mw-100 { max-width:100% !important; }
   * }}}
  */
  def `mw-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-0 {
   *   margin-right:0 !important;
   *   margin-left:0 !important;
   * }
   * }}}
  */
  def `mx-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-1 {
   *   margin-right:.25rem !important;
   *   margin-left:.25rem !important;
   * }
   * }}}
  */
  def `mx-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-2 {
   *   margin-right:.5rem !important;
   *   margin-left:.5rem !important;
   * }
   * }}}
  */
  def `mx-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-3 {
   *   margin-right:1rem !important;
   *   margin-left:1rem !important;
   * }
   * }}}
  */
  def `mx-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-4 {
   *   margin-right:1.5rem !important;
   *   margin-left:1.5rem !important;
   * }
   * }}}
  */
  def `mx-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-5 {
   *   margin-right:2rem !important;
   *   margin-left:2rem !important;
   * }
   * }}}
  */
  def `mx-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-6 {
   *   margin-right:3rem !important;
   *   margin-left:3rem !important;
   * }
   * }}}
  */
  def `mx-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-7 {
   *   margin-right:5rem !important;
   *   margin-left:5rem !important;
   * }
   * }}}
  */
  def `mx-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-8 {
   *   margin-right:8rem !important;
   *   margin-left:8rem !important;
   * }
   * }}}
  */
  def `mx-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-auto {
   *   margin-right:auto !important;
   *   margin-left:auto !important;
   * }
   * }}}
  */
  def `mx-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-0 {
   *   margin-right:0 !important;
   *   margin-left:0 !important;
   * }
   * }}}
  */
  def `mx-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-1 {
   *   margin-right:.25rem !important;
   *   margin-left:.25rem !important;
   * }
   * }}}
  */
  def `mx-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-2 {
   *   margin-right:.5rem !important;
   *   margin-left:.5rem !important;
   * }
   * }}}
  */
  def `mx-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-3 {
   *   margin-right:1rem !important;
   *   margin-left:1rem !important;
   * }
   * }}}
  */
  def `mx-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-4 {
   *   margin-right:1.5rem !important;
   *   margin-left:1.5rem !important;
   * }
   * }}}
  */
  def `mx-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-5 {
   *   margin-right:2rem !important;
   *   margin-left:2rem !important;
   * }
   * }}}
  */
  def `mx-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-6 {
   *   margin-right:3rem !important;
   *   margin-left:3rem !important;
   * }
   * }}}
  */
  def `mx-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-7 {
   *   margin-right:5rem !important;
   *   margin-left:5rem !important;
   * }
   * }}}
  */
  def `mx-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-8 {
   *   margin-right:8rem !important;
   *   margin-left:8rem !important;
   * }
   * }}}
  */
  def `mx-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-lg-auto {
   *   margin-right:auto !important;
   *   margin-left:auto !important;
   * }
   * }}}
  */
  def `mx-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-0 {
   *   margin-right:0 !important;
   *   margin-left:0 !important;
   * }
   * }}}
  */
  def `mx-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-1 {
   *   margin-right:.25rem !important;
   *   margin-left:.25rem !important;
   * }
   * }}}
  */
  def `mx-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-2 {
   *   margin-right:.5rem !important;
   *   margin-left:.5rem !important;
   * }
   * }}}
  */
  def `mx-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-3 {
   *   margin-right:1rem !important;
   *   margin-left:1rem !important;
   * }
   * }}}
  */
  def `mx-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-4 {
   *   margin-right:1.5rem !important;
   *   margin-left:1.5rem !important;
   * }
   * }}}
  */
  def `mx-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-5 {
   *   margin-right:2rem !important;
   *   margin-left:2rem !important;
   * }
   * }}}
  */
  def `mx-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-6 {
   *   margin-right:3rem !important;
   *   margin-left:3rem !important;
   * }
   * }}}
  */
  def `mx-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-7 {
   *   margin-right:5rem !important;
   *   margin-left:5rem !important;
   * }
   * }}}
  */
  def `mx-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-8 {
   *   margin-right:8rem !important;
   *   margin-left:8rem !important;
   * }
   * }}}
  */
  def `mx-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-md-auto {
   *   margin-right:auto !important;
   *   margin-left:auto !important;
   * }
   * }}}
  */
  def `mx-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-0 {
   *   margin-right:0 !important;
   *   margin-left:0 !important;
   * }
   * }}}
  */
  def `mx-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-1 {
   *   margin-right:.25rem !important;
   *   margin-left:.25rem !important;
   * }
   * }}}
  */
  def `mx-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-2 {
   *   margin-right:.5rem !important;
   *   margin-left:.5rem !important;
   * }
   * }}}
  */
  def `mx-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-3 {
   *   margin-right:1rem !important;
   *   margin-left:1rem !important;
   * }
   * }}}
  */
  def `mx-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-4 {
   *   margin-right:1.5rem !important;
   *   margin-left:1.5rem !important;
   * }
   * }}}
  */
  def `mx-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-5 {
   *   margin-right:2rem !important;
   *   margin-left:2rem !important;
   * }
   * }}}
  */
  def `mx-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-6 {
   *   margin-right:3rem !important;
   *   margin-left:3rem !important;
   * }
   * }}}
  */
  def `mx-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-7 {
   *   margin-right:5rem !important;
   *   margin-left:5rem !important;
   * }
   * }}}
  */
  def `mx-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-8 {
   *   margin-right:8rem !important;
   *   margin-left:8rem !important;
   * }
   * }}}
  */
  def `mx-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-sm-auto {
   *   margin-right:auto !important;
   *   margin-left:auto !important;
   * }
   * }}}
  */
  def `mx-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-0 {
   *   margin-right:0 !important;
   *   margin-left:0 !important;
   * }
   * }}}
  */
  def `mx-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-1 {
   *   margin-right:.25rem !important;
   *   margin-left:.25rem !important;
   * }
   * }}}
  */
  def `mx-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-2 {
   *   margin-right:.5rem !important;
   *   margin-left:.5rem !important;
   * }
   * }}}
  */
  def `mx-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-3 {
   *   margin-right:1rem !important;
   *   margin-left:1rem !important;
   * }
   * }}}
  */
  def `mx-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-4 {
   *   margin-right:1.5rem !important;
   *   margin-left:1.5rem !important;
   * }
   * }}}
  */
  def `mx-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-5 {
   *   margin-right:2rem !important;
   *   margin-left:2rem !important;
   * }
   * }}}
  */
  def `mx-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-6 {
   *   margin-right:3rem !important;
   *   margin-left:3rem !important;
   * }
   * }}}
  */
  def `mx-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-7 {
   *   margin-right:5rem !important;
   *   margin-left:5rem !important;
   * }
   * }}}
  */
  def `mx-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-8 {
   *   margin-right:8rem !important;
   *   margin-left:8rem !important;
   * }
   * }}}
  */
  def `mx-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xl-auto {
   *   margin-right:auto !important;
   *   margin-left:auto !important;
   * }
   * }}}
  */
  def `mx-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-0 {
   *   margin-right:0 !important;
   *   margin-left:0 !important;
   * }
   * }}}
  */
  def `mx-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-1 {
   *   margin-right:.25rem !important;
   *   margin-left:.25rem !important;
   * }
   * }}}
  */
  def `mx-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-2 {
   *   margin-right:.5rem !important;
   *   margin-left:.5rem !important;
   * }
   * }}}
  */
  def `mx-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-3 {
   *   margin-right:1rem !important;
   *   margin-left:1rem !important;
   * }
   * }}}
  */
  def `mx-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-4 {
   *   margin-right:1.5rem !important;
   *   margin-left:1.5rem !important;
   * }
   * }}}
  */
  def `mx-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-5 {
   *   margin-right:2rem !important;
   *   margin-left:2rem !important;
   * }
   * }}}
  */
  def `mx-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-6 {
   *   margin-right:3rem !important;
   *   margin-left:3rem !important;
   * }
   * }}}
  */
  def `mx-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-7 {
   *   margin-right:5rem !important;
   *   margin-left:5rem !important;
   * }
   * }}}
  */
  def `mx-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-8 {
   *   margin-right:8rem !important;
   *   margin-left:8rem !important;
   * }
   * }}}
  */
  def `mx-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .mx-xxl-auto {
   *   margin-right:auto !important;
   *   margin-left:auto !important;
   * }
   * }}}
  */
  def `mx-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-0 {
   *   margin-top:0 !important;
   *   margin-bottom:0 !important;
   * }
   * }}}
  */
  def `my-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-1 {
   *   margin-top:.25rem !important;
   *   margin-bottom:.25rem !important;
   * }
   * }}}
  */
  def `my-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-2 {
   *   margin-top:.5rem !important;
   *   margin-bottom:.5rem !important;
   * }
   * }}}
  */
  def `my-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-3 {
   *   margin-top:1rem !important;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `my-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-4 {
   *   margin-top:1.5rem !important;
   *   margin-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `my-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-5 {
   *   margin-top:2rem !important;
   *   margin-bottom:2rem !important;
   * }
   * }}}
  */
  def `my-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-6 {
   *   margin-top:3rem !important;
   *   margin-bottom:3rem !important;
   * }
   * }}}
  */
  def `my-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-7 {
   *   margin-top:5rem !important;
   *   margin-bottom:5rem !important;
   * }
   * }}}
  */
  def `my-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-8 {
   *   margin-top:8rem !important;
   *   margin-bottom:8rem !important;
   * }
   * }}}
  */
  def `my-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-auto {
   *   margin-top:auto !important;
   *   margin-bottom:auto !important;
   * }
   * }}}
  */
  def `my-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-0 {
   *   margin-top:0 !important;
   *   margin-bottom:0 !important;
   * }
   * }}}
  */
  def `my-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-1 {
   *   margin-top:.25rem !important;
   *   margin-bottom:.25rem !important;
   * }
   * }}}
  */
  def `my-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-2 {
   *   margin-top:.5rem !important;
   *   margin-bottom:.5rem !important;
   * }
   * }}}
  */
  def `my-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-3 {
   *   margin-top:1rem !important;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `my-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-4 {
   *   margin-top:1.5rem !important;
   *   margin-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `my-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-5 {
   *   margin-top:2rem !important;
   *   margin-bottom:2rem !important;
   * }
   * }}}
  */
  def `my-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-6 {
   *   margin-top:3rem !important;
   *   margin-bottom:3rem !important;
   * }
   * }}}
  */
  def `my-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-7 {
   *   margin-top:5rem !important;
   *   margin-bottom:5rem !important;
   * }
   * }}}
  */
  def `my-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-8 {
   *   margin-top:8rem !important;
   *   margin-bottom:8rem !important;
   * }
   * }}}
  */
  def `my-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-lg-auto {
   *   margin-top:auto !important;
   *   margin-bottom:auto !important;
   * }
   * }}}
  */
  def `my-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-0 {
   *   margin-top:0 !important;
   *   margin-bottom:0 !important;
   * }
   * }}}
  */
  def `my-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-1 {
   *   margin-top:.25rem !important;
   *   margin-bottom:.25rem !important;
   * }
   * }}}
  */
  def `my-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-2 {
   *   margin-top:.5rem !important;
   *   margin-bottom:.5rem !important;
   * }
   * }}}
  */
  def `my-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-3 {
   *   margin-top:1rem !important;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `my-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-4 {
   *   margin-top:1.5rem !important;
   *   margin-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `my-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-5 {
   *   margin-top:2rem !important;
   *   margin-bottom:2rem !important;
   * }
   * }}}
  */
  def `my-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-6 {
   *   margin-top:3rem !important;
   *   margin-bottom:3rem !important;
   * }
   * }}}
  */
  def `my-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-7 {
   *   margin-top:5rem !important;
   *   margin-bottom:5rem !important;
   * }
   * }}}
  */
  def `my-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-8 {
   *   margin-top:8rem !important;
   *   margin-bottom:8rem !important;
   * }
   * }}}
  */
  def `my-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-md-auto {
   *   margin-top:auto !important;
   *   margin-bottom:auto !important;
   * }
   * }}}
  */
  def `my-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-0 {
   *   margin-top:0 !important;
   *   margin-bottom:0 !important;
   * }
   * }}}
  */
  def `my-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-1 {
   *   margin-top:.25rem !important;
   *   margin-bottom:.25rem !important;
   * }
   * }}}
  */
  def `my-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-2 {
   *   margin-top:.5rem !important;
   *   margin-bottom:.5rem !important;
   * }
   * }}}
  */
  def `my-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-3 {
   *   margin-top:1rem !important;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `my-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-4 {
   *   margin-top:1.5rem !important;
   *   margin-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `my-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-5 {
   *   margin-top:2rem !important;
   *   margin-bottom:2rem !important;
   * }
   * }}}
  */
  def `my-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-6 {
   *   margin-top:3rem !important;
   *   margin-bottom:3rem !important;
   * }
   * }}}
  */
  def `my-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-7 {
   *   margin-top:5rem !important;
   *   margin-bottom:5rem !important;
   * }
   * }}}
  */
  def `my-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-8 {
   *   margin-top:8rem !important;
   *   margin-bottom:8rem !important;
   * }
   * }}}
  */
  def `my-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-sm-auto {
   *   margin-top:auto !important;
   *   margin-bottom:auto !important;
   * }
   * }}}
  */
  def `my-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-0 {
   *   margin-top:0 !important;
   *   margin-bottom:0 !important;
   * }
   * }}}
  */
  def `my-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-1 {
   *   margin-top:.25rem !important;
   *   margin-bottom:.25rem !important;
   * }
   * }}}
  */
  def `my-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-2 {
   *   margin-top:.5rem !important;
   *   margin-bottom:.5rem !important;
   * }
   * }}}
  */
  def `my-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-3 {
   *   margin-top:1rem !important;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `my-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-4 {
   *   margin-top:1.5rem !important;
   *   margin-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `my-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-5 {
   *   margin-top:2rem !important;
   *   margin-bottom:2rem !important;
   * }
   * }}}
  */
  def `my-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-6 {
   *   margin-top:3rem !important;
   *   margin-bottom:3rem !important;
   * }
   * }}}
  */
  def `my-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-7 {
   *   margin-top:5rem !important;
   *   margin-bottom:5rem !important;
   * }
   * }}}
  */
  def `my-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-8 {
   *   margin-top:8rem !important;
   *   margin-bottom:8rem !important;
   * }
   * }}}
  */
  def `my-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xl-auto {
   *   margin-top:auto !important;
   *   margin-bottom:auto !important;
   * }
   * }}}
  */
  def `my-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-0 {
   *   margin-top:0 !important;
   *   margin-bottom:0 !important;
   * }
   * }}}
  */
  def `my-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-1 {
   *   margin-top:.25rem !important;
   *   margin-bottom:.25rem !important;
   * }
   * }}}
  */
  def `my-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-2 {
   *   margin-top:.5rem !important;
   *   margin-bottom:.5rem !important;
   * }
   * }}}
  */
  def `my-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-3 {
   *   margin-top:1rem !important;
   *   margin-bottom:1rem !important;
   * }
   * }}}
  */
  def `my-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-4 {
   *   margin-top:1.5rem !important;
   *   margin-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `my-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-5 {
   *   margin-top:2rem !important;
   *   margin-bottom:2rem !important;
   * }
   * }}}
  */
  def `my-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-6 {
   *   margin-top:3rem !important;
   *   margin-bottom:3rem !important;
   * }
   * }}}
  */
  def `my-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-7 {
   *   margin-top:5rem !important;
   *   margin-bottom:5rem !important;
   * }
   * }}}
  */
  def `my-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-8 {
   *   margin-top:8rem !important;
   *   margin-bottom:8rem !important;
   * }
   * }}}
  */
  def `my-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .my-xxl-auto {
   *   margin-top:auto !important;
   *   margin-bottom:auto !important;
   * }
   * }}}
  */
  def `my-xxl-auto`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .na,
   * .highlight .p { color:#ffe484; }
   * }}}
  */
  def na: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav {
   *   --tblr-nav-link-padding-x:0.75rem;
   *   --tblr-nav-link-padding-y:0.5rem;
   *   --tblr-nav-link-color:var(--tblr-secondary);
   *   --tblr-nav-link-hover-color:var(--tblr-link-hover-color);
   *   --tblr-nav-link-disabled-color:var(--tblr-disabled-color);
   *   display:flex;
   *   flex-wrap:wrap;
   *   padding-left:0;
   *   margin-bottom:0;
   *   list-style:none;
   * }
   * }}}
  */
  def nav: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-header-tabs .nav-bordered { border:0; }
   * }}}
  */
  def `nav-bordered`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-fill .nav-item,
   * .nav-fill>.nav-link {
   *   flex:1 1 auto;
   *   text-align:center;
   * }
   * }}}
  */
  def `nav-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-tabs .nav-item.show .nav-link,
   * .nav-tabs .nav-link.active {
   *   color:var(--tblr-nav-tabs-link-active-color);
   *   background-color:var(--tblr-nav-tabs-link-active-bg);
   *   border-color:var(--tblr-nav-tabs-link-active-border-color);
   * }
   * }}}
  */
  def `nav-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-justified .nav-item,
   * .nav-justified>.nav-link {
   *   flex-basis:0;
   *   flex-grow:1;
   *   text-align:center;
   * }
   * }}}
  */
  def `nav-justified`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-link {
   *   display:block;
   *   padding:var(--tblr-nav-link-padding-y) var(--tblr-nav-link-padding-x);
   *   font-size:var(--tblr-nav-link-font-size);
   *   font-weight:var(--tblr-nav-link-font-weight);
   *   color:var(--tblr-nav-link-color);
   *   background:0 0;
   *   border:0;
   *   transition:color .15s ease-in-out,background-color .15s ease-in-out,border-color .15s ease-in-out;
   * }
   * }}}
  */
  def `nav-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-link-icon {
   *   width:1.25rem;
   *   height:1.25rem;
   *   margin-right:.5rem;
   *   color:var(--tblr-icon-color);
   * }
   * }}}
  */
  def `nav-link-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-link-toggle {
   *   margin-left:auto;
   *   padding:0 .25rem;
   *   transition:transform .3s;
   * }
   * }}}
  */
  def `nav-link-toggle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-pills {
   *   --tblr-nav-pills-border-radius:var(--tblr-border-radius);
   *   --tblr-nav-pills-link-active-color:var(--tblr-primary);
   *   --tblr-nav-pills-link-active-bg:var(--tblr-active-bg);
   * }
   * }}}
  */
  def `nav-pills`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-tabs {
   *   --tblr-nav-tabs-border-width:var(--tblr-border-width);
   *   --tblr-nav-tabs-border-color:var(--tblr-border-color);
   *   --tblr-nav-tabs-border-radius:var(--tblr-border-radius);
   *   --tblr-nav-tabs-link-hover-border-color:var(--tblr-border-color) var(--tblr-border-color) var(--tblr-border-color);
   *   --tblr-nav-tabs-link-active-color:var(--tblr-body-color);
   *   --tblr-nav-tabs-link-active-bg:var(--tblr-body-bg);
   *   --tblr-nav-tabs-link-active-border-color:var(--tblr-border-color) var(--tblr-border-color) var(--tblr-border-color);
   *   border-bottom:var(--tblr-nav-tabs-border-width) solid var(--tblr-nav-tabs-border-color);
   * }
   * }}}
  */
  def `nav-tabs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .card-tabs .nav-tabs-bottom { margin-bottom:0; }
   * }}}
  */
  def `nav-tabs-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-underline {
   *   --tblr-nav-underline-gap:1rem;
   *   --tblr-nav-underline-border-width:0.125rem;
   *   --tblr-nav-underline-link-active-color:var(--tblr-emphasis-color);
   *   gap:var(--tblr-nav-underline-gap);
   * }
   * }}}
  */
  def `nav-underline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .nav-vertical,
   * .nav-vertical .nav {
   *   flex-direction:column;
   *   flex-wrap:nowrap;
   * }
   * }}}
  */
  def `nav-vertical`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar {
   *   --tblr-navbar-padding-x:0;
   *   --tblr-navbar-padding-y:0.25rem;
   *   --tblr-navbar-color:var(--tblr-body-color);
   *   --tblr-navbar-hover-color:rgba(var(--tblr-emphasis-color-rgb),0.8);
   *   --tblr-navbar-disabled-color:var(--tblr-disabled-color);
   *   --tblr-navbar-active-color:var(--tblr-body-color) color;
   *   --tblr-navbar-brand-padding-y:0.5rem;
   *   --tblr-navbar-brand-margin-end:1rem;
   *   --tblr-navbar-brand-font-size:1.25rem;
   *   --tblr-navbar-brand-color:var(--tblr-body-color);
   *   --tblr-navbar-brand-hover-color:var(--tblr-body-color) color;
   *   --tblr-navbar-nav-link-padding-x:0.75rem;
   *   --tblr-navbar-toggler-padding-y:0;
   *   --tblr-navbar-toggler-padding-x:0;
   *   --tblr-navbar-toggler-font-size:1rem;
   *   --tblr-navbar-toggler-icon-bg:url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%2824, 36, 51, 0.75%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e");
   *   --tblr-navbar-toggler-border-color:rgba(var(--tblr-emphasis-color-rgb),0.15);
   *   --tblr-navbar-toggler-border-radius:var(--tblr-border-radius);
   *   --tblr-navbar-toggler-focus-width:0;
   *   --tblr-navbar-toggler-transition:box-shadow 0.15s ease-in-out;
   *   position:relative;
   *   display:flex;
   *   flex-wrap:wrap;
   *   align-items:center;
   *   justify-content:space-between;
   *   padding:var(--tblr-navbar-padding-y) var(--tblr-navbar-padding-x);
   * }
   * }}}
  */
  def navbar: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-brand {
   *   padding-top:var(--tblr-navbar-brand-padding-y);
   *   padding-bottom:var(--tblr-navbar-brand-padding-y);
   *   margin-right:var(--tblr-navbar-brand-margin-end);
   *   font-size:var(--tblr-navbar-brand-font-size);
   *   color:var(--tblr-navbar-brand-color);
   *   white-space:nowrap;
   * }
   * }}}
  */
  def `navbar-brand`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * [data-bs-theme=dark] .navbar-brand-autodark .navbar-brand-image { filter:brightness(0) invert(1); }
   * }}}
  */
  def `navbar-brand-autodark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-brand-image {
   *   height:2rem;
   *   width:auto;
   * }
   * }}}
  */
  def `navbar-brand-image`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-collapse {
   *   flex-basis:100%;
   *   flex-grow:1;
   *   align-items:center;
   * }
   * }}}
  */
  def `navbar-collapse`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-dark,
   * .navbar[data-bs-theme=dark],
   * body[data-bs-theme=dark] .navbar[data-bs-theme=light] {
   *   --tblr-navbar-color:rgba(255,255,255,0.7);
   *   --tblr-navbar-hover-color:rgba(255,255,255,0.75);
   *   --tblr-navbar-disabled-color:var(--tblr-disabled-color);
   *   --tblr-navbar-active-color:#ffffff;
   *   --tblr-navbar-brand-color:#ffffff;
   *   --tblr-navbar-brand-hover-color:#ffffff;
   *   --tblr-navbar-toggler-border-color:rgba(255,255,255,0.1);
   *   --tblr-navbar-toggler-icon-bg:url("data:image/svg+xml,%3csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 30 30'%3e%3cpath stroke='rgba%28255, 255, 255, 0.7%29' stroke-linecap='round' stroke-miterlimit='10' stroke-width='2' d='M4 7h22M4 15h22M4 23h22'/%3e%3c/svg%3e");
   * }
   * }}}
  */
  def `navbar-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand {
   *   flex-wrap:nowrap;
   *   justify-content:flex-start;
   * }
   * }}}
  */
  def `navbar-expand`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-lg .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def `navbar-expand-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-md .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def `navbar-expand-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def `navbar-expand-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-xl .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def `navbar-expand-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-xxl .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def `navbar-expand-xxl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-nav {
   *   --tblr-nav-link-padding-x:0;
   *   --tblr-nav-link-padding-y:0.5rem;
   *   --tblr-nav-link-color:var(--tblr-navbar-color);
   *   --tblr-nav-link-hover-color:var(--tblr-navbar-hover-color);
   *   --tblr-nav-link-disabled-color:var(--tblr-navbar-disabled-color);
   *   display:flex;
   *   flex-direction:column;
   *   padding-left:0;
   *   margin-bottom:0;
   *   list-style:none;
   * }
   * }}}
  */
  def `navbar-nav`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-nav-scroll {
   *   max-height:var(--tblr-scroll-height,75vh);
   *   overflow-y:auto;
   * }
   * }}}
  */
  def `navbar-nav-scroll`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-overlap:after {
   *   content:"";
   *   height:9rem;
   *   position:absolute;
   *   top:100%;
   *   left:0;
   *   right:0;
   *   background:inherit;
   *   z-index:-1;
   *   box-shadow:inherit;
   * }
   * }}}
  */
  def `navbar-overlap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm.navbar-vertical.navbar-right~.navbar,
   * .navbar-expand-sm.navbar-vertical.navbar-right~.page-wrapper {
   *   margin-left:0;
   *   margin-right:15rem;
   * }
   * }}}
  */
  def `navbar-right`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-side {
   *   margin:0;
   *   display:flex;
   *   flex-direction:row;
   *   align-items:center;
   *   justify-content:space-around;
   * }
   * }}}
  */
  def `navbar-side`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-text {
   *   padding-top:.5rem;
   *   padding-bottom:.5rem;
   *   color:var(--tblr-navbar-color);
   * }
   * }}}
  */
  def `navbar-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-toggler {
   *   padding:var(--tblr-navbar-toggler-padding-y) var(--tblr-navbar-toggler-padding-x);
   *   font-size:var(--tblr-navbar-toggler-font-size);
   *   line-height:1;
   *   color:var(--tblr-navbar-color);
   *   background-color:transparent;
   *   border:var(--tblr-border-width) solid var(--tblr-navbar-toggler-border-color);
   *   border-radius:var(--tblr-navbar-toggler-border-radius);
   *   transition:var(--tblr-navbar-toggler-transition);
   * }
   * }}}
  */
  def `navbar-toggler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-toggler-icon {
   *   display:inline-block;
   *   width:1.5em;
   *   height:1.5em;
   *   vertical-align:middle;
   *   background-image:var(--tblr-navbar-toggler-icon-bg);
   *   background-repeat:no-repeat;
   *   background-position:center;
   *   background-size:100%;
   * }
   * }}}
  */
  def `navbar-toggler-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-transparent {
   *   --tblr-navbar-border-color:transparent !important;
   *   background:0 0 !important;
   * }
   * }}}
  */
  def `navbar-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm.navbar-vertical.navbar-right~.navbar,
   * .navbar-expand-sm.navbar-vertical.navbar-right~.page-wrapper {
   *   margin-left:0;
   *   margin-right:15rem;
   * }
   * }}}
  */
  def `navbar-vertical`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .nc,
   * .highlight .nt,
   * .highlight .nx { color:#ff8383; }
   * }}}
  */
  def nc: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-date.next-month,
   * .calendar-date.prev-month { opacity:.25; }
   * }}}
  */
  def `next-month`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown .create,
   * .ts-dropdown .no-results,
   * .ts-dropdown .optgroup-header,
   * .ts-dropdown .option { padding:3px .75rem; }
   * }}}
  */
  def `no-results`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .no-scroll { overflow:hidden; }
   * }}}
  */
  def `no-scroll`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-active { box-shadow:inset 0 0 1px #fff,inset 0 1px 7px #ddd,0 3px 6px -3px #bbb; }
   * }}}
  */
  def `noUi-active`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-base,
   * .noUi-connects {
   *   width:100%;
   *   height:100%;
   *   position:relative;
   *   z-index:1;
   * }
   * }}}
  */
  def `noUi-base`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-connect,
   * .noUi-origin {
   *   will-change:transform;
   *   position:absolute;
   *   z-index:1;
   *   top:0;
   *   right:0;
   *   height:100%;
   *   width:100%;
   *   -ms-transform-origin:0 0;
   *   -webkit-transform-origin:0 0;
   *   -webkit-transform-style:preserve-3d;
   *   transform-origin:0 0;
   *   transform-style:flat;
   * }
   * }}}
  */
  def `noUi-connect`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-base,
   * .noUi-connects {
   *   width:100%;
   *   height:100%;
   *   position:relative;
   *   z-index:1;
   * }
   * }}}
  */
  def `noUi-connects`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-draggable { cursor:ew-resize; }
   * }}}
  */
  def `noUi-draggable`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-handle {
   *   -webkit-backface-visibility:hidden;
   *   backface-visibility:hidden;
   *   position:absolute;
   * }
   * }}}
  */
  def `noUi-handle`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-txt-dir-rtl.noUi-horizontal .noUi-origin {
   *   left:0;
   *   right:auto;
   * }
   * }}}
  */
  def `noUi-horizontal`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-marker {
   *   position:absolute;
   *   background:#ccc;
   * }
   * }}}
  */
  def `noUi-marker`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-marker-horizontal.noUi-marker {
   *   margin-left:-1px;
   *   width:2px;
   *   height:5px;
   * }
   * }}}
  */
  def `noUi-marker-horizontal`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-marker-large { background:#aaa; }
   * }}}
  */
  def `noUi-marker-large`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-marker-sub { background:#aaa; }
   * }}}
  */
  def `noUi-marker-sub`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-marker-vertical.noUi-marker {
   *   width:5px;
   *   height:2px;
   *   margin-top:-1px;
   * }
   * }}}
  */
  def `noUi-marker-vertical`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-connect,
   * .noUi-origin {
   *   will-change:transform;
   *   position:absolute;
   *   z-index:1;
   *   top:0;
   *   right:0;
   *   height:100%;
   *   width:100%;
   *   -ms-transform-origin:0 0;
   *   -webkit-transform-origin:0 0;
   *   -webkit-transform-style:preserve-3d;
   *   transform-origin:0 0;
   *   transform-style:flat;
   * }
   * }}}
  */
  def `noUi-origin`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-pips,
   * .noUi-pips * { box-sizing:border-box; }
   * }}}
  */
  def `noUi-pips`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-pips-horizontal {
   *   padding:10px 0;
   *   height:80px;
   *   top:100%;
   *   left:0;
   *   width:100%;
   * }
   * }}}
  */
  def `noUi-pips-horizontal`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-pips-vertical {
   *   padding:0 10px;
   *   height:100%;
   *   top:0;
   *   left:100%;
   * }
   * }}}
  */
  def `noUi-pips-vertical`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-rtl .noUi-value-horizontal { transform:translate(50%,50%); }
   * }}}
  */
  def `noUi-rtl`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-state-drag * { cursor:inherit !important; }
   * }}}
  */
  def `noUi-state-drag`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-state-tap .noUi-connect,
   * .noUi-state-tap .noUi-origin { transition:transform .3s; }
   * }}}
  */
  def `noUi-state-tap`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-target,
   * .noUi-target * {
   *   -webkit-touch-callout:none;
   *   -webkit-tap-highlight-color:transparent;
   *   -webkit-user-select:none;
   *   touch-action:none;
   *   -ms-user-select:none;
   *   -moz-user-select:none;
   *   user-select:none;
   *   box-sizing:border-box;
   * }
   * }}}
  */
  def `noUi-target`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-tooltip {
   *   display:block;
   *   position:absolute;
   *   border:1px solid #d9d9d9;
   *   border-radius:3px;
   *   background:#fff;
   *   color:#000;
   *   padding:5px;
   *   text-align:center;
   *   white-space:nowrap;
   * }
   * }}}
  */
  def `noUi-tooltip`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-touch-area {
   *   height:100%;
   *   width:100%;
   * }
   * }}}
  */
  def `noUi-touch-area`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-txt-dir-rtl.noUi-horizontal .noUi-origin {
   *   left:0;
   *   right:auto;
   * }
   * }}}
  */
  def `noUi-txt-dir-rtl`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-value {
   *   position:absolute;
   *   white-space:nowrap;
   *   text-align:center;
   * }
   * }}}
  */
  def `noUi-value`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-value-horizontal { transform:translate(-50%,50%); }
   * }}}
  */
  def `noUi-value-horizontal`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-value-sub {
   *   color:#ccc;
   *   font-size:10px;
   * }
   * }}}
  */
  def `noUi-value-sub`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-value-vertical {
   *   transform:translate(0,-50%);
   *   padding-left:25px;
   * }
   * }}}
  */
  def `noUi-value-vertical`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .noUi-vertical .noUi-origin {
   *   top:-100%;
   *   width:0;
   * }
   * }}}
  */
  def `noUi-vertical`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .nc,
   * .highlight .nt,
   * .highlight .nx { color:#ff8383; }
   * }}}
  */
  def nt: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .nc,
   * .highlight .nt,
   * .highlight .nx { color:#ff8383; }
   * }}}
  */
  def nx: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-fit-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-fit-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fit-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-lg-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-fit-lg-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-lg-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-fit-lg-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-lg-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fit-lg-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-lg-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-fit-lg-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-lg-scale {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-fit-lg-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-md-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-fit-md-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-md-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-fit-md-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-md-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fit-md-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-md-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-fit-md-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-md-scale {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-fit-md-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-fit-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-scale {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-fit-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-sm-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-fit-sm-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-sm-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-fit-sm-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-sm-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fit-sm-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-sm-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-fit-sm-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-sm-scale {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-fit-sm-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xl-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-fit-xl-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xl-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-fit-xl-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xl-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fit-xl-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xl-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-fit-xl-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xl-scale {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-fit-xl-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xxl-contain {
   *   -o-object-fit:contain !important;
   *   object-fit:contain !important;
   * }
   * }}}
  */
  def `object-fit-xxl-contain`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xxl-cover {
   *   -o-object-fit:cover !important;
   *   object-fit:cover !important;
   * }
   * }}}
  */
  def `object-fit-xxl-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xxl-fill {
   *   -o-object-fit:fill !important;
   *   object-fit:fill !important;
   * }
   * }}}
  */
  def `object-fit-xxl-fill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xxl-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-fit-xxl-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-fit-xxl-scale {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-fit-xxl-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-none {
   *   -o-object-fit:none !important;
   *   object-fit:none !important;
   * }
   * }}}
  */
  def `object-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .object-scale-down {
   *   -o-object-fit:scale-down !important;
   *   object-fit:scale-down !important;
   * }
   * }}}
  */
  def `object-scale-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def offcanvas: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-backdrop {
   *   position:fixed;
   *   top:0;
   *   left:0;
   *   z-index:1040;
   *   width:100vw;
   *   height:100vh;
   *   background-color:#182433;
   * }
   * }}}
  */
  def `offcanvas-backdrop`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm .offcanvas .offcanvas-body {
   *   display:flex;
   *   flex-grow:0;
   *   padding:0;
   *   overflow-y:visible;
   * }
   * }}}
  */
  def `offcanvas-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-sm.offcanvas-bottom {
   *   right:0;
   *   left:0;
   *   height:var(--tblr-offcanvas-height);
   *   max-height:100%;
   *   border-top:var(--tblr-offcanvas-border-width) solid var(--tblr-offcanvas-border-color);
   *   transform:translateY(100%);
   * }
   * }}}
  */
  def `offcanvas-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-sm.offcanvas-end {
   *   top:0;
   *   right:0;
   *   width:var(--tblr-offcanvas-width);
   *   border-left:var(--tblr-offcanvas-border-width) solid var(--tblr-offcanvas-border-color);
   *   transform:translateX(100%);
   * }
   * }}}
  */
  def `offcanvas-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-footer { padding:1.5rem 1.5rem; }
   * }}}
  */
  def `offcanvas-footer`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm .offcanvas .offcanvas-header { display:none; }
   * }}}
  */
  def `offcanvas-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas,
   * .offcanvas-lg,
   * .offcanvas-md,
   * .offcanvas-sm,
   * .offcanvas-xl,
   * .offcanvas-xxl {
   *   --tblr-offcanvas-zindex:1045;
   *   --tblr-offcanvas-width:400px;
   *   --tblr-offcanvas-height:30vh;
   *   --tblr-offcanvas-padding-x:1.5rem;
   *   --tblr-offcanvas-padding-y:1.5rem;
   *   --tblr-offcanvas-color:var(--tblr-body-color);
   *   --tblr-offcanvas-bg:var(--tblr-bg-surface);
   *   --tblr-offcanvas-border-width:var(--tblr-border-width);
   *   --tblr-offcanvas-border-color:var(--tblr-border-color);
   *   --tblr-offcanvas-box-shadow:0 0.125rem 0.25rem rgba(0,0,0,0.075);
   *   --tblr-offcanvas-transition:transform 0.3s ease-in-out;
   *   --tblr-offcanvas-title-line-height:1.4285714286;
   * }
   * }}}
  */
  def `offcanvas-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas,
   * .offcanvas-lg,
   * .offcanvas-md,
   * .offcanvas-sm,
   * .offcanvas-xl,
   * .offcanvas-xxl {
   *   --tblr-offcanvas-zindex:1045;
   *   --tblr-offcanvas-width:400px;
   *   --tblr-offcanvas-height:30vh;
   *   --tblr-offcanvas-padding-x:1.5rem;
   *   --tblr-offcanvas-padding-y:1.5rem;
   *   --tblr-offcanvas-color:var(--tblr-body-color);
   *   --tblr-offcanvas-bg:var(--tblr-bg-surface);
   *   --tblr-offcanvas-border-width:var(--tblr-border-width);
   *   --tblr-offcanvas-border-color:var(--tblr-border-color);
   *   --tblr-offcanvas-box-shadow:0 0.125rem 0.25rem rgba(0,0,0,0.075);
   *   --tblr-offcanvas-transition:transform 0.3s ease-in-out;
   *   --tblr-offcanvas-title-line-height:1.4285714286;
   * }
   * }}}
  */
  def `offcanvas-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-narrow { width:20rem; }
   * }}}
  */
  def `offcanvas-narrow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas,
   * .offcanvas-lg,
   * .offcanvas-md,
   * .offcanvas-sm,
   * .offcanvas-xl,
   * .offcanvas-xxl {
   *   --tblr-offcanvas-zindex:1045;
   *   --tblr-offcanvas-width:400px;
   *   --tblr-offcanvas-height:30vh;
   *   --tblr-offcanvas-padding-x:1.5rem;
   *   --tblr-offcanvas-padding-y:1.5rem;
   *   --tblr-offcanvas-color:var(--tblr-body-color);
   *   --tblr-offcanvas-bg:var(--tblr-bg-surface);
   *   --tblr-offcanvas-border-width:var(--tblr-border-width);
   *   --tblr-offcanvas-border-color:var(--tblr-border-color);
   *   --tblr-offcanvas-box-shadow:0 0.125rem 0.25rem rgba(0,0,0,0.075);
   *   --tblr-offcanvas-transition:transform 0.3s ease-in-out;
   *   --tblr-offcanvas-title-line-height:1.4285714286;
   * }
   * }}}
  */
  def `offcanvas-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-sm.offcanvas-start {
   *   top:0;
   *   left:0;
   *   width:var(--tblr-offcanvas-width);
   *   border-right:var(--tblr-offcanvas-border-width) solid var(--tblr-offcanvas-border-color);
   *   transform:translateX(-100%);
   * }
   * }}}
  */
  def `offcanvas-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-title {
   *   margin-bottom:0;
   *   line-height:var(--tblr-offcanvas-title-line-height);
   * }
   * }}}
  */
  def `offcanvas-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas-sm.offcanvas-top {
   *   top:0;
   *   right:0;
   *   left:0;
   *   height:var(--tblr-offcanvas-height);
   *   max-height:100%;
   *   border-bottom:var(--tblr-offcanvas-border-width) solid var(--tblr-offcanvas-border-color);
   *   transform:translateY(-100%);
   * }
   * }}}
  */
  def `offcanvas-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas,
   * .offcanvas-lg,
   * .offcanvas-md,
   * .offcanvas-sm,
   * .offcanvas-xl,
   * .offcanvas-xxl {
   *   --tblr-offcanvas-zindex:1045;
   *   --tblr-offcanvas-width:400px;
   *   --tblr-offcanvas-height:30vh;
   *   --tblr-offcanvas-padding-x:1.5rem;
   *   --tblr-offcanvas-padding-y:1.5rem;
   *   --tblr-offcanvas-color:var(--tblr-body-color);
   *   --tblr-offcanvas-bg:var(--tblr-bg-surface);
   *   --tblr-offcanvas-border-width:var(--tblr-border-width);
   *   --tblr-offcanvas-border-color:var(--tblr-border-color);
   *   --tblr-offcanvas-box-shadow:0 0.125rem 0.25rem rgba(0,0,0,0.075);
   *   --tblr-offcanvas-transition:transform 0.3s ease-in-out;
   *   --tblr-offcanvas-title-line-height:1.4285714286;
   * }
   * }}}
  */
  def `offcanvas-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offcanvas,
   * .offcanvas-lg,
   * .offcanvas-md,
   * .offcanvas-sm,
   * .offcanvas-xl,
   * .offcanvas-xxl {
   *   --tblr-offcanvas-zindex:1045;
   *   --tblr-offcanvas-width:400px;
   *   --tblr-offcanvas-height:30vh;
   *   --tblr-offcanvas-padding-x:1.5rem;
   *   --tblr-offcanvas-padding-y:1.5rem;
   *   --tblr-offcanvas-color:var(--tblr-body-color);
   *   --tblr-offcanvas-bg:var(--tblr-bg-surface);
   *   --tblr-offcanvas-border-width:var(--tblr-border-width);
   *   --tblr-offcanvas-border-color:var(--tblr-border-color);
   *   --tblr-offcanvas-box-shadow:0 0.125rem 0.25rem rgba(0,0,0,0.075);
   *   --tblr-offcanvas-transition:transform 0.3s ease-in-out;
   *   --tblr-offcanvas-title-line-height:1.4285714286;
   * }
   * }}}
  */
  def `offcanvas-xxl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-1 { margin-left:8.33333333%; }
   * }}}
  */
  def `offset-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-10 { margin-left:83.33333333%; }
   * }}}
  */
  def `offset-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-11 { margin-left:91.66666667%; }
   * }}}
  */
  def `offset-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-2 { margin-left:16.66666667%; }
   * }}}
  */
  def `offset-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-3 { margin-left:25%; }
   * }}}
  */
  def `offset-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-4 { margin-left:33.33333333%; }
   * }}}
  */
  def `offset-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-5 { margin-left:41.66666667%; }
   * }}}
  */
  def `offset-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-6 { margin-left:50%; }
   * }}}
  */
  def `offset-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-7 { margin-left:58.33333333%; }
   * }}}
  */
  def `offset-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-8 { margin-left:66.66666667%; }
   * }}}
  */
  def `offset-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-9 { margin-left:75%; }
   * }}}
  */
  def `offset-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-0 { margin-left:0; }
   * }}}
  */
  def `offset-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-1 { margin-left:8.33333333%; }
   * }}}
  */
  def `offset-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-10 { margin-left:83.33333333%; }
   * }}}
  */
  def `offset-lg-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-11 { margin-left:91.66666667%; }
   * }}}
  */
  def `offset-lg-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-2 { margin-left:16.66666667%; }
   * }}}
  */
  def `offset-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-3 { margin-left:25%; }
   * }}}
  */
  def `offset-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-4 { margin-left:33.33333333%; }
   * }}}
  */
  def `offset-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-5 { margin-left:41.66666667%; }
   * }}}
  */
  def `offset-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-6 { margin-left:50%; }
   * }}}
  */
  def `offset-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-7 { margin-left:58.33333333%; }
   * }}}
  */
  def `offset-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-8 { margin-left:66.66666667%; }
   * }}}
  */
  def `offset-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-lg-9 { margin-left:75%; }
   * }}}
  */
  def `offset-lg-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-0 { margin-left:0; }
   * }}}
  */
  def `offset-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-1 { margin-left:8.33333333%; }
   * }}}
  */
  def `offset-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-10 { margin-left:83.33333333%; }
   * }}}
  */
  def `offset-md-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-11 { margin-left:91.66666667%; }
   * }}}
  */
  def `offset-md-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-2 { margin-left:16.66666667%; }
   * }}}
  */
  def `offset-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-3 { margin-left:25%; }
   * }}}
  */
  def `offset-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-4 { margin-left:33.33333333%; }
   * }}}
  */
  def `offset-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-5 { margin-left:41.66666667%; }
   * }}}
  */
  def `offset-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-6 { margin-left:50%; }
   * }}}
  */
  def `offset-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-7 { margin-left:58.33333333%; }
   * }}}
  */
  def `offset-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-8 { margin-left:66.66666667%; }
   * }}}
  */
  def `offset-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-md-9 { margin-left:75%; }
   * }}}
  */
  def `offset-md-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-0 { margin-left:0; }
   * }}}
  */
  def `offset-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-1 { margin-left:8.33333333%; }
   * }}}
  */
  def `offset-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-10 { margin-left:83.33333333%; }
   * }}}
  */
  def `offset-sm-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-11 { margin-left:91.66666667%; }
   * }}}
  */
  def `offset-sm-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-2 { margin-left:16.66666667%; }
   * }}}
  */
  def `offset-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-3 { margin-left:25%; }
   * }}}
  */
  def `offset-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-4 { margin-left:33.33333333%; }
   * }}}
  */
  def `offset-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-5 { margin-left:41.66666667%; }
   * }}}
  */
  def `offset-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-6 { margin-left:50%; }
   * }}}
  */
  def `offset-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-7 { margin-left:58.33333333%; }
   * }}}
  */
  def `offset-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-8 { margin-left:66.66666667%; }
   * }}}
  */
  def `offset-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-sm-9 { margin-left:75%; }
   * }}}
  */
  def `offset-sm-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-0 { margin-left:0; }
   * }}}
  */
  def `offset-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-1 { margin-left:8.33333333%; }
   * }}}
  */
  def `offset-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-10 { margin-left:83.33333333%; }
   * }}}
  */
  def `offset-xl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-11 { margin-left:91.66666667%; }
   * }}}
  */
  def `offset-xl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-2 { margin-left:16.66666667%; }
   * }}}
  */
  def `offset-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-3 { margin-left:25%; }
   * }}}
  */
  def `offset-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-4 { margin-left:33.33333333%; }
   * }}}
  */
  def `offset-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-5 { margin-left:41.66666667%; }
   * }}}
  */
  def `offset-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-6 { margin-left:50%; }
   * }}}
  */
  def `offset-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-7 { margin-left:58.33333333%; }
   * }}}
  */
  def `offset-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-8 { margin-left:66.66666667%; }
   * }}}
  */
  def `offset-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xl-9 { margin-left:75%; }
   * }}}
  */
  def `offset-xl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-0 { margin-left:0; }
   * }}}
  */
  def `offset-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-1 { margin-left:8.33333333%; }
   * }}}
  */
  def `offset-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-10 { margin-left:83.33333333%; }
   * }}}
  */
  def `offset-xxl-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-11 { margin-left:91.66666667%; }
   * }}}
  */
  def `offset-xxl-11`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-2 { margin-left:16.66666667%; }
   * }}}
  */
  def `offset-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-3 { margin-left:25%; }
   * }}}
  */
  def `offset-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-4 { margin-left:33.33333333%; }
   * }}}
  */
  def `offset-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-5 { margin-left:41.66666667%; }
   * }}}
  */
  def `offset-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-6 { margin-left:50%; }
   * }}}
  */
  def `offset-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-7 { margin-left:58.33333333%; }
   * }}}
  */
  def `offset-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-8 { margin-left:66.66666667%; }
   * }}}
  */
  def `offset-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .offset-xxl-9 { margin-left:75%; }
   * }}}
  */
  def `offset-xxl-9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-0 { opacity:0 !important; }
   * }}}
  */
  def `opacity-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-10 { opacity:.1 !important; }
   * }}}
  */
  def `opacity-10`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-100 { opacity:1 !important; }
   * }}}
  */
  def `opacity-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-15 { opacity:.15 !important; }
   * }}}
  */
  def `opacity-15`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-20 { opacity:.2 !important; }
   * }}}
  */
  def `opacity-20`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-25 { opacity:.25 !important; }
   * }}}
  */
  def `opacity-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-30 { opacity:.3 !important; }
   * }}}
  */
  def `opacity-30`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-35 { opacity:.35 !important; }
   * }}}
  */
  def `opacity-35`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-40 { opacity:.4 !important; }
   * }}}
  */
  def `opacity-40`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-45 { opacity:.45 !important; }
   * }}}
  */
  def `opacity-45`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-5 { opacity:.05 !important; }
   * }}}
  */
  def `opacity-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-50 { opacity:.5 !important; }
   * }}}
  */
  def `opacity-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-55 { opacity:.55 !important; }
   * }}}
  */
  def `opacity-55`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-60 { opacity:.6 !important; }
   * }}}
  */
  def `opacity-60`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-65 { opacity:.65 !important; }
   * }}}
  */
  def `opacity-65`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-70 { opacity:.7 !important; }
   * }}}
  */
  def `opacity-70`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-75 { opacity:.75 !important; }
   * }}}
  */
  def `opacity-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-80 { opacity:.8 !important; }
   * }}}
  */
  def `opacity-80`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-85 { opacity:.85 !important; }
   * }}}
  */
  def `opacity-85`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-90 { opacity:.9 !important; }
   * }}}
  */
  def `opacity-90`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .opacity-95 { opacity:.95 !important; }
   * }}}
  */
  def `opacity-95`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown.plugin-optgroup_columns .optgroup {
   *   border-right:1px solid #f2f2f2;
   *   border-top:0 none;
   *   flex-grow:1;
   *   flex-basis:0;
   *   min-width:0;
   * }
   * }}}
  */
  def optgroup: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown.plugin-optgroup_columns .optgroup-header { border-top:0 none; }
   * }}}
  */
  def `optgroup-header`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-checkbox_options .option input { margin-right:.5rem; }
   * }}}
  */
  def option: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-0 { order:0 !important; }
   * }}}
  */
  def `order-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-1 { order:1 !important; }
   * }}}
  */
  def `order-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-2 { order:2 !important; }
   * }}}
  */
  def `order-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-3 { order:3 !important; }
   * }}}
  */
  def `order-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-4 { order:4 !important; }
   * }}}
  */
  def `order-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-5 { order:5 !important; }
   * }}}
  */
  def `order-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-first { order:-1 !important; }
   * }}}
  */
  def `order-first`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-last { order:6 !important; }
   * }}}
  */
  def `order-last`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-0 { order:0 !important; }
   * }}}
  */
  def `order-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-1 { order:1 !important; }
   * }}}
  */
  def `order-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-2 { order:2 !important; }
   * }}}
  */
  def `order-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-3 { order:3 !important; }
   * }}}
  */
  def `order-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-4 { order:4 !important; }
   * }}}
  */
  def `order-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-5 { order:5 !important; }
   * }}}
  */
  def `order-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-first { order:-1 !important; }
   * }}}
  */
  def `order-lg-first`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-lg-last { order:6 !important; }
   * }}}
  */
  def `order-lg-last`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-0 { order:0 !important; }
   * }}}
  */
  def `order-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-1 { order:1 !important; }
   * }}}
  */
  def `order-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-2 { order:2 !important; }
   * }}}
  */
  def `order-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-3 { order:3 !important; }
   * }}}
  */
  def `order-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-4 { order:4 !important; }
   * }}}
  */
  def `order-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-5 { order:5 !important; }
   * }}}
  */
  def `order-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-first { order:-1 !important; }
   * }}}
  */
  def `order-md-first`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-md-last { order:6 !important; }
   * }}}
  */
  def `order-md-last`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-0 { order:0 !important; }
   * }}}
  */
  def `order-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-1 { order:1 !important; }
   * }}}
  */
  def `order-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-2 { order:2 !important; }
   * }}}
  */
  def `order-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-3 { order:3 !important; }
   * }}}
  */
  def `order-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-4 { order:4 !important; }
   * }}}
  */
  def `order-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-5 { order:5 !important; }
   * }}}
  */
  def `order-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-first { order:-1 !important; }
   * }}}
  */
  def `order-sm-first`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-sm-last { order:6 !important; }
   * }}}
  */
  def `order-sm-last`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-0 { order:0 !important; }
   * }}}
  */
  def `order-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-1 { order:1 !important; }
   * }}}
  */
  def `order-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-2 { order:2 !important; }
   * }}}
  */
  def `order-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-3 { order:3 !important; }
   * }}}
  */
  def `order-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-4 { order:4 !important; }
   * }}}
  */
  def `order-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-5 { order:5 !important; }
   * }}}
  */
  def `order-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-first { order:-1 !important; }
   * }}}
  */
  def `order-xl-first`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xl-last { order:6 !important; }
   * }}}
  */
  def `order-xl-last`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-0 { order:0 !important; }
   * }}}
  */
  def `order-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-1 { order:1 !important; }
   * }}}
  */
  def `order-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-2 { order:2 !important; }
   * }}}
  */
  def `order-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-3 { order:3 !important; }
   * }}}
  */
  def `order-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-4 { order:4 !important; }
   * }}}
  */
  def `order-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-5 { order:5 !important; }
   * }}}
  */
  def `order-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-first { order:-1 !important; }
   * }}}
  */
  def `order-xxl-first`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .order-xxl-last { order:6 !important; }
   * }}}
  */
  def `order-xxl-last`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-auto { overflow:auto !important; }
   * }}}
  */
  def `overflow-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-hidden { overflow:hidden !important; }
   * }}}
  */
  def `overflow-hidden`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-scroll { overflow:scroll !important; }
   * }}}
  */
  def `overflow-scroll`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-visible { overflow:visible !important; }
   * }}}
  */
  def `overflow-visible`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-x-auto { overflow-x:auto !important; }
   * }}}
  */
  def `overflow-x-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-x-hidden { overflow-x:hidden !important; }
   * }}}
  */
  def `overflow-x-hidden`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-x-scroll { overflow-x:scroll !important; }
   * }}}
  */
  def `overflow-x-scroll`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-x-visible { overflow-x:visible !important; }
   * }}}
  */
  def `overflow-x-visible`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-y-auto { overflow-y:auto !important; }
   * }}}
  */
  def `overflow-y-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-y-hidden { overflow-y:hidden !important; }
   * }}}
  */
  def `overflow-y-hidden`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-y-scroll { overflow-y:scroll !important; }
   * }}}
  */
  def `overflow-y-scroll`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .overflow-y-visible { overflow-y:visible !important; }
   * }}}
  */
  def `overflow-y-visible`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .na,
   * .highlight .p { color:#ffe484; }
   * }}}
  */
  def p: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-0 { padding:0 !important; }
   * }}}
  */
  def `p-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-1 { padding:.25rem !important; }
   * }}}
  */
  def `p-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-2 { padding:.5rem !important; }
   * }}}
  */
  def `p-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-3 { padding:1rem !important; }
   * }}}
  */
  def `p-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-4 { padding:1.5rem !important; }
   * }}}
  */
  def `p-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-5 { padding:2rem !important; }
   * }}}
  */
  def `p-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-6 { padding:3rem !important; }
   * }}}
  */
  def `p-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-7 { padding:5rem !important; }
   * }}}
  */
  def `p-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-8 { padding:8rem !important; }
   * }}}
  */
  def `p-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-0 { padding:0 !important; }
   * }}}
  */
  def `p-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-1 { padding:.25rem !important; }
   * }}}
  */
  def `p-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-2 { padding:.5rem !important; }
   * }}}
  */
  def `p-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-3 { padding:1rem !important; }
   * }}}
  */
  def `p-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-4 { padding:1.5rem !important; }
   * }}}
  */
  def `p-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-5 { padding:2rem !important; }
   * }}}
  */
  def `p-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-6 { padding:3rem !important; }
   * }}}
  */
  def `p-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-7 { padding:5rem !important; }
   * }}}
  */
  def `p-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-lg-8 { padding:8rem !important; }
   * }}}
  */
  def `p-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-0 { padding:0 !important; }
   * }}}
  */
  def `p-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-1 { padding:.25rem !important; }
   * }}}
  */
  def `p-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-2 { padding:.5rem !important; }
   * }}}
  */
  def `p-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-3 { padding:1rem !important; }
   * }}}
  */
  def `p-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-4 { padding:1.5rem !important; }
   * }}}
  */
  def `p-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-5 { padding:2rem !important; }
   * }}}
  */
  def `p-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-6 { padding:3rem !important; }
   * }}}
  */
  def `p-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-7 { padding:5rem !important; }
   * }}}
  */
  def `p-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-md-8 { padding:8rem !important; }
   * }}}
  */
  def `p-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-0 { padding:0 !important; }
   * }}}
  */
  def `p-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-1 { padding:.25rem !important; }
   * }}}
  */
  def `p-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-2 { padding:.5rem !important; }
   * }}}
  */
  def `p-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-3 { padding:1rem !important; }
   * }}}
  */
  def `p-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-4 { padding:1.5rem !important; }
   * }}}
  */
  def `p-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-5 { padding:2rem !important; }
   * }}}
  */
  def `p-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-6 { padding:3rem !important; }
   * }}}
  */
  def `p-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-7 { padding:5rem !important; }
   * }}}
  */
  def `p-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-sm-8 { padding:8rem !important; }
   * }}}
  */
  def `p-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-0 { padding:0 !important; }
   * }}}
  */
  def `p-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-1 { padding:.25rem !important; }
   * }}}
  */
  def `p-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-2 { padding:.5rem !important; }
   * }}}
  */
  def `p-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-3 { padding:1rem !important; }
   * }}}
  */
  def `p-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-4 { padding:1.5rem !important; }
   * }}}
  */
  def `p-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-5 { padding:2rem !important; }
   * }}}
  */
  def `p-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-6 { padding:3rem !important; }
   * }}}
  */
  def `p-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-7 { padding:5rem !important; }
   * }}}
  */
  def `p-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xl-8 { padding:8rem !important; }
   * }}}
  */
  def `p-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-0 { padding:0 !important; }
   * }}}
  */
  def `p-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-1 { padding:.25rem !important; }
   * }}}
  */
  def `p-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-2 { padding:.5rem !important; }
   * }}}
  */
  def `p-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-3 { padding:1rem !important; }
   * }}}
  */
  def `p-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-4 { padding:1.5rem !important; }
   * }}}
  */
  def `p-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-5 { padding:2rem !important; }
   * }}}
  */
  def `p-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-6 { padding:3rem !important; }
   * }}}
  */
  def `p-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-7 { padding:5rem !important; }
   * }}}
  */
  def `p-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .p-xxl-8 { padding:8rem !important; }
   * }}}
  */
  def `p-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .layout-boxed .page {
   *   margin:0 auto;
   *   max-width:var(--tblr-theme-boxed-width);
   *   border-radius:var(--tblr-theme-boxed-border-radius);
   *   color:var(--tblr-body-color);
   * }
   * }}}
  */
  def page: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-wrapper-full .page-body:first-child {
   *   margin:0;
   *   border-top:0;
   * }
   * }}}
  */
  def `page-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-body-card {
   *   background:var(--tblr-bg-surface);
   *   border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color-translucent);
   *   padding:var(--tblr-page-padding) 0;
   *   margin-bottom:0;
   *   flex:1;
   * }
   * }}}
  */
  def `page-body-card`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-center .container {
   *   margin-top:auto;
   *   margin-bottom:auto;
   * }
   * }}}
  */
  def `page-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-cover {
   *   background:no-repeat center/cover;
   *   min-height:9rem;
   * }
   * }}}
  */
  def `page-cover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-cover-img {
   *   position:absolute;
   *   top:calc(-2*var(--tblr-page-cover-blur,0));
   *   left:calc(-2*var(--tblr-page-cover-blur,0));
   *   right:calc(-2*var(--tblr-page-cover-blur,0));
   *   bottom:calc(-2*var(--tblr-page-cover-blur,0));
   *   pointer-events:none;
   *   filter:blur(var(--tblr-page-cover-blur));
   *   -o-object-fit:cover;
   *   object-fit:cover;
   *   background-size:cover;
   *   background-position:center;
   *   z-index:-1;
   * }
   * }}}
  */
  def `page-cover-img`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-cover-overlay { position:relative; }
   * }}}
  */
  def `page-cover-overlay`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .example-content .page-header { margin-bottom:0; }
   * }}}
  */
  def `page-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-header-border {
   *   border-bottom:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   padding:var(--tblr-page-padding-y) 0;
   *   margin:0 !important;
   *   background-color:var(--tblr-bg-surface);
   * }
   * }}}
  */
  def `page-header-border`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-header-tabs .nav-bordered { border:0; }
   * }}}
  */
  def `page-header-tabs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-item:not(:first-child) .page-link { margin-left:calc(0*-1); }
   * }}}
  */
  def `page-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-item-subtitle {
   *   margin-bottom:2px;
   *   font-size:12px;
   *   color:var(--tblr-secondary);
   *   text-transform:uppercase;
   * }
   * }}}
  */
  def `page-item-subtitle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-item-title {
   *   font-size:1rem;
   *   font-weight:var(--tblr-font-weight-normal);
   *   color:var(--tblr-body-color);
   * }
   * }}}
  */
  def `page-item-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-link {
   *   position:relative;
   *   display:block;
   *   padding:var(--tblr-pagination-padding-y) var(--tblr-pagination-padding-x);
   *   font-size:var(--tblr-pagination-font-size);
   *   color:var(--tblr-pagination-color);
   *   background-color:var(--tblr-pagination-bg);
   *   border:var(--tblr-pagination-border-width) solid var(--tblr-pagination-border-color);
   *   transition:color .15s ease-in-out,background-color .15s ease-in-out,border-color .15s ease-in-out,box-shadow .15s ease-in-out;
   * }
   * }}}
  */
  def `page-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-item.page-next,
   * .page-item.page-prev {
   *   flex:0 0 50%;
   *   text-align:left;
   * }
   * }}}
  */
  def `page-next`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-pretitle {
   *   font-size:.625rem;
   *   font-weight:var(--tblr-font-weight-bold);
   *   text-transform:uppercase;
   *   letter-spacing:.04em;
   *   line-height:1rem;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `page-pretitle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-item.page-next,
   * .page-item.page-prev {
   *   flex:0 0 50%;
   *   text-align:left;
   * }
   * }}}
  */
  def `page-prev`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-subtitle {
   *   margin-top:.25rem;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def `page-subtitle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-tabs {
   *   margin-top:.5rem;
   *   position:relative;
   * }
   * }}}
  */
  def `page-tabs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-title {
   *   margin:0;
   *   font-size:var(--tblr-font-size-h2);
   *   line-height:var(--tblr-line-height-h4);
   *   font-weight:var(--tblr-font-weight-headings);
   *   color:inherit;
   *   display:flex;
   *   align-items:center;
   * }
   * }}}
  */
  def `page-title`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-title-lg {
   *   font-size:1.5rem;
   *   line-height:2rem;
   * }
   * }}}
  */
  def `page-title-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .navbar-expand-sm.navbar-vertical.navbar-right~.navbar,
   * .navbar-expand-sm.navbar-vertical.navbar-right~.page-wrapper {
   *   margin-left:0;
   *   margin-right:15rem;
   * }
   * }}}
  */
  def `page-wrapper`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .page-wrapper-full .page-body:first-child {
   *   margin:0;
   *   border-top:0;
   * }
   * }}}
  */
  def `page-wrapper-full`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pagination {
   *   --tblr-pagination-padding-x:0.25rem;
   *   --tblr-pagination-padding-y:0.25rem;
   *   --tblr-pagination-font-size:0.875rem;
   *   --tblr-pagination-color:var(--tblr-secondary);
   *   --tblr-pagination-bg:transparent;
   *   --tblr-pagination-border-width:0;
   *   --tblr-pagination-border-color:var(--tblr-border-color);
   *   --tblr-pagination-border-radius:var(--tblr-border-radius);
   *   --tblr-pagination-hover-color:var(--tblr-link-hover-color);
   *   --tblr-pagination-hover-bg:var(--tblr-tertiary-bg);
   *   --tblr-pagination-hover-border-color:var(--tblr-border-color);
   *   --tblr-pagination-focus-color:var(--tblr-link-hover-color);
   *   --tblr-pagination-focus-bg:var(--tblr-secondary-bg);
   *   --tblr-pagination-focus-box-shadow:0 0 0 0.25rem rgba(var(--tblr-primary-rgb),0.25);
   *   --tblr-pagination-active-color:#ffffff;
   *   --tblr-pagination-active-bg:var(--tblr-primary);
   *   --tblr-pagination-active-border-color:var(--tblr-primary);
   *   --tblr-pagination-disabled-color:var(--tblr-disabled-color);
   *   --tblr-pagination-disabled-bg:transparent;
   *   --tblr-pagination-disabled-border-color:var(--tblr-border-color);
   *   display:flex;
   *   padding-left:0;
   *   list-style:none;
   * }
   * }}}
  */
  def pagination: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pagination-lg {
   *   --tblr-pagination-padding-x:1.5rem;
   *   --tblr-pagination-padding-y:0.75rem;
   *   --tblr-pagination-font-size:1.09375rem;
   *   --tblr-pagination-border-radius:var(--tblr-border-radius-lg);
   * }
   * }}}
  */
  def `pagination-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pagination-sm {
   *   --tblr-pagination-padding-x:0.5rem;
   *   --tblr-pagination-padding-y:0.25rem;
   *   --tblr-pagination-font-size:0.765625rem;
   *   --tblr-pagination-border-radius:var(--tblr-border-radius-sm);
   * }
   * }}}
  */
  def `pagination-sm`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment {
   *   height:2.5rem;
   *   aspect-ratio:1.66666;
   *   display:inline-block;
   *   background:no-repeat center/100% 100%;
   *   vertical-align:bottom;
   *   font-style:normal;
   *   box-shadow:0 0 1px 1px rgba(0,0,0,.1);
   *   border-radius:2px;
   * }
   * }}}
  */
  def payment: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-2xl { height:7rem; }
   * }}}
  */
  def `payment-2xl`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-lg { height:3rem; }
   * }}}
  */
  def `payment-lg`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-md { height:2.5rem; }
   * }}}
  */
  def `payment-md`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-2checkout { background-image:url(../img/payments/2checkout.svg); }
   * }}}
  */
  def `payment-provider-2checkout`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-2checkout-dark { background-image:url(../img/payments/2checkout-dark.svg); }
   * }}}
  */
  def `payment-provider-2checkout-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-alipay { background-image:url(../img/payments/alipay.svg); }
   * }}}
  */
  def `payment-provider-alipay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-alipay-dark { background-image:url(../img/payments/alipay-dark.svg); }
   * }}}
  */
  def `payment-provider-alipay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-amazon { background-image:url(../img/payments/amazon.svg); }
   * }}}
  */
  def `payment-provider-amazon`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-amazon-dark { background-image:url(../img/payments/amazon-dark.svg); }
   * }}}
  */
  def `payment-provider-amazon-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-americanexpress { background-image:url(../img/payments/americanexpress.svg); }
   * }}}
  */
  def `payment-provider-americanexpress`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-americanexpress-dark { background-image:url(../img/payments/americanexpress-dark.svg); }
   * }}}
  */
  def `payment-provider-americanexpress-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-applepay { background-image:url(../img/payments/applepay.svg); }
   * }}}
  */
  def `payment-provider-applepay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-applepay-dark { background-image:url(../img/payments/applepay-dark.svg); }
   * }}}
  */
  def `payment-provider-applepay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-bancontact { background-image:url(../img/payments/bancontact.svg); }
   * }}}
  */
  def `payment-provider-bancontact`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-bancontact-dark { background-image:url(../img/payments/bancontact-dark.svg); }
   * }}}
  */
  def `payment-provider-bancontact-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-bitcoin { background-image:url(../img/payments/bitcoin.svg); }
   * }}}
  */
  def `payment-provider-bitcoin`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-bitcoin-dark { background-image:url(../img/payments/bitcoin-dark.svg); }
   * }}}
  */
  def `payment-provider-bitcoin-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-bitpay { background-image:url(../img/payments/bitpay.svg); }
   * }}}
  */
  def `payment-provider-bitpay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-bitpay-dark { background-image:url(../img/payments/bitpay-dark.svg); }
   * }}}
  */
  def `payment-provider-bitpay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-blik { background-image:url(../img/payments/blik.svg); }
   * }}}
  */
  def `payment-provider-blik`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-blik-dark { background-image:url(../img/payments/blik-dark.svg); }
   * }}}
  */
  def `payment-provider-blik-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-cirrus { background-image:url(../img/payments/cirrus.svg); }
   * }}}
  */
  def `payment-provider-cirrus`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-cirrus-dark { background-image:url(../img/payments/cirrus-dark.svg); }
   * }}}
  */
  def `payment-provider-cirrus-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-clickandbuy { background-image:url(../img/payments/clickandbuy.svg); }
   * }}}
  */
  def `payment-provider-clickandbuy`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-clickandbuy-dark { background-image:url(../img/payments/clickandbuy-dark.svg); }
   * }}}
  */
  def `payment-provider-clickandbuy-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-coinkite { background-image:url(../img/payments/coinkite.svg); }
   * }}}
  */
  def `payment-provider-coinkite`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-coinkite-dark { background-image:url(../img/payments/coinkite-dark.svg); }
   * }}}
  */
  def `payment-provider-coinkite-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-dinersclub { background-image:url(../img/payments/dinersclub.svg); }
   * }}}
  */
  def `payment-provider-dinersclub`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-dinersclub-dark { background-image:url(../img/payments/dinersclub-dark.svg); }
   * }}}
  */
  def `payment-provider-dinersclub-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-directdebit { background-image:url(../img/payments/directdebit.svg); }
   * }}}
  */
  def `payment-provider-directdebit`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-directdebit-dark { background-image:url(../img/payments/directdebit-dark.svg); }
   * }}}
  */
  def `payment-provider-directdebit-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-discover { background-image:url(../img/payments/discover.svg); }
   * }}}
  */
  def `payment-provider-discover`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-discover-dark { background-image:url(../img/payments/discover-dark.svg); }
   * }}}
  */
  def `payment-provider-discover-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-dotpay { background-image:url(../img/payments/dotpay.svg); }
   * }}}
  */
  def `payment-provider-dotpay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-dotpay-dark { background-image:url(../img/payments/dotpay-dark.svg); }
   * }}}
  */
  def `payment-provider-dotpay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-dwolla { background-image:url(../img/payments/dwolla.svg); }
   * }}}
  */
  def `payment-provider-dwolla`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-dwolla-dark { background-image:url(../img/payments/dwolla-dark.svg); }
   * }}}
  */
  def `payment-provider-dwolla-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ebay { background-image:url(../img/payments/ebay.svg); }
   * }}}
  */
  def `payment-provider-ebay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ebay-dark { background-image:url(../img/payments/ebay-dark.svg); }
   * }}}
  */
  def `payment-provider-ebay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-epayco { background-image:url(../img/payments/epayco.svg); }
   * }}}
  */
  def `payment-provider-epayco`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-epayco-dark { background-image:url(../img/payments/epayco-dark.svg); }
   * }}}
  */
  def `payment-provider-epayco-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-eway { background-image:url(../img/payments/eway.svg); }
   * }}}
  */
  def `payment-provider-eway`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-eway-dark { background-image:url(../img/payments/eway-dark.svg); }
   * }}}
  */
  def `payment-provider-eway-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-giropay { background-image:url(../img/payments/giropay.svg); }
   * }}}
  */
  def `payment-provider-giropay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-giropay-dark { background-image:url(../img/payments/giropay-dark.svg); }
   * }}}
  */
  def `payment-provider-giropay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-googlewallet { background-image:url(../img/payments/googlewallet.svg); }
   * }}}
  */
  def `payment-provider-googlewallet`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-googlewallet-dark { background-image:url(../img/payments/googlewallet-dark.svg); }
   * }}}
  */
  def `payment-provider-googlewallet-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ingenico { background-image:url(../img/payments/ingenico.svg); }
   * }}}
  */
  def `payment-provider-ingenico`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ingenico-dark { background-image:url(../img/payments/ingenico-dark.svg); }
   * }}}
  */
  def `payment-provider-ingenico-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-jcb { background-image:url(../img/payments/jcb.svg); }
   * }}}
  */
  def `payment-provider-jcb`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-jcb-dark { background-image:url(../img/payments/jcb-dark.svg); }
   * }}}
  */
  def `payment-provider-jcb-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-klarna { background-image:url(../img/payments/klarna.svg); }
   * }}}
  */
  def `payment-provider-klarna`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-klarna-dark { background-image:url(../img/payments/klarna-dark.svg); }
   * }}}
  */
  def `payment-provider-klarna-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-laser { background-image:url(../img/payments/laser.svg); }
   * }}}
  */
  def `payment-provider-laser`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-laser-dark { background-image:url(../img/payments/laser-dark.svg); }
   * }}}
  */
  def `payment-provider-laser-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-maestro { background-image:url(../img/payments/maestro.svg); }
   * }}}
  */
  def `payment-provider-maestro`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-maestro-dark { background-image:url(../img/payments/maestro-dark.svg); }
   * }}}
  */
  def `payment-provider-maestro-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-mastercard { background-image:url(../img/payments/mastercard.svg); }
   * }}}
  */
  def `payment-provider-mastercard`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-mastercard-dark { background-image:url(../img/payments/mastercard-dark.svg); }
   * }}}
  */
  def `payment-provider-mastercard-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-mir { background-image:url(../img/payments/mir.svg); }
   * }}}
  */
  def `payment-provider-mir`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-mir-dark { background-image:url(../img/payments/mir-dark.svg); }
   * }}}
  */
  def `payment-provider-mir-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-monero { background-image:url(../img/payments/monero.svg); }
   * }}}
  */
  def `payment-provider-monero`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-monero-dark { background-image:url(../img/payments/monero-dark.svg); }
   * }}}
  */
  def `payment-provider-monero-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-neteller { background-image:url(../img/payments/neteller.svg); }
   * }}}
  */
  def `payment-provider-neteller`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-neteller-dark { background-image:url(../img/payments/neteller-dark.svg); }
   * }}}
  */
  def `payment-provider-neteller-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ogone { background-image:url(../img/payments/ogone.svg); }
   * }}}
  */
  def `payment-provider-ogone`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ogone-dark { background-image:url(../img/payments/ogone-dark.svg); }
   * }}}
  */
  def `payment-provider-ogone-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-okpay { background-image:url(../img/payments/okpay.svg); }
   * }}}
  */
  def `payment-provider-okpay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-okpay-dark { background-image:url(../img/payments/okpay-dark.svg); }
   * }}}
  */
  def `payment-provider-okpay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paybox { background-image:url(../img/payments/paybox.svg); }
   * }}}
  */
  def `payment-provider-paybox`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paybox-dark { background-image:url(../img/payments/paybox-dark.svg); }
   * }}}
  */
  def `payment-provider-paybox-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paymill { background-image:url(../img/payments/paymill.svg); }
   * }}}
  */
  def `payment-provider-paymill`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paymill-dark { background-image:url(../img/payments/paymill-dark.svg); }
   * }}}
  */
  def `payment-provider-paymill-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payone { background-image:url(../img/payments/payone.svg); }
   * }}}
  */
  def `payment-provider-payone`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payone-dark { background-image:url(../img/payments/payone-dark.svg); }
   * }}}
  */
  def `payment-provider-payone-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payoneer { background-image:url(../img/payments/payoneer.svg); }
   * }}}
  */
  def `payment-provider-payoneer`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payoneer-dark { background-image:url(../img/payments/payoneer-dark.svg); }
   * }}}
  */
  def `payment-provider-payoneer-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paypal { background-image:url(../img/payments/paypal.svg); }
   * }}}
  */
  def `payment-provider-paypal`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paypal-dark { background-image:url(../img/payments/paypal-dark.svg); }
   * }}}
  */
  def `payment-provider-paypal-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paysafecard { background-image:url(../img/payments/paysafecard.svg); }
   * }}}
  */
  def `payment-provider-paysafecard`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-paysafecard-dark { background-image:url(../img/payments/paysafecard-dark.svg); }
   * }}}
  */
  def `payment-provider-paysafecard-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payu { background-image:url(../img/payments/payu.svg); }
   * }}}
  */
  def `payment-provider-payu`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payu-dark { background-image:url(../img/payments/payu-dark.svg); }
   * }}}
  */
  def `payment-provider-payu-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payza { background-image:url(../img/payments/payza.svg); }
   * }}}
  */
  def `payment-provider-payza`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-payza-dark { background-image:url(../img/payments/payza-dark.svg); }
   * }}}
  */
  def `payment-provider-payza-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-przelewy24 { background-image:url(../img/payments/przelewy24.svg); }
   * }}}
  */
  def `payment-provider-przelewy24`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-przelewy24-dark { background-image:url(../img/payments/przelewy24-dark.svg); }
   * }}}
  */
  def `payment-provider-przelewy24-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ripple { background-image:url(../img/payments/ripple.svg); }
   * }}}
  */
  def `payment-provider-ripple`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ripple-dark { background-image:url(../img/payments/ripple-dark.svg); }
   * }}}
  */
  def `payment-provider-ripple-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-sage { background-image:url(../img/payments/sage.svg); }
   * }}}
  */
  def `payment-provider-sage`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-sage-dark { background-image:url(../img/payments/sage-dark.svg); }
   * }}}
  */
  def `payment-provider-sage-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-sepa { background-image:url(../img/payments/sepa.svg); }
   * }}}
  */
  def `payment-provider-sepa`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-sepa-dark { background-image:url(../img/payments/sepa-dark.svg); }
   * }}}
  */
  def `payment-provider-sepa-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-shopify { background-image:url(../img/payments/shopify.svg); }
   * }}}
  */
  def `payment-provider-shopify`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-shopify-dark { background-image:url(../img/payments/shopify-dark.svg); }
   * }}}
  */
  def `payment-provider-shopify-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-skrill { background-image:url(../img/payments/skrill.svg); }
   * }}}
  */
  def `payment-provider-skrill`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-skrill-dark { background-image:url(../img/payments/skrill-dark.svg); }
   * }}}
  */
  def `payment-provider-skrill-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-solo { background-image:url(../img/payments/solo.svg); }
   * }}}
  */
  def `payment-provider-solo`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-solo-dark { background-image:url(../img/payments/solo-dark.svg); }
   * }}}
  */
  def `payment-provider-solo-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-square { background-image:url(../img/payments/square.svg); }
   * }}}
  */
  def `payment-provider-square`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-square-dark { background-image:url(../img/payments/square-dark.svg); }
   * }}}
  */
  def `payment-provider-square-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-stripe { background-image:url(../img/payments/stripe.svg); }
   * }}}
  */
  def `payment-provider-stripe`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-stripe-dark { background-image:url(../img/payments/stripe-dark.svg); }
   * }}}
  */
  def `payment-provider-stripe-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-switch { background-image:url(../img/payments/switch.svg); }
   * }}}
  */
  def `payment-provider-switch`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-switch-dark { background-image:url(../img/payments/switch-dark.svg); }
   * }}}
  */
  def `payment-provider-switch-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-tpay { background-image:url(../img/payments/tpay.svg); }
   * }}}
  */
  def `payment-provider-tpay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-tpay-dark { background-image:url(../img/payments/tpay-dark.svg); }
   * }}}
  */
  def `payment-provider-tpay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ukash { background-image:url(../img/payments/ukash.svg); }
   * }}}
  */
  def `payment-provider-ukash`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-ukash-dark { background-image:url(../img/payments/ukash-dark.svg); }
   * }}}
  */
  def `payment-provider-ukash-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-unionpay { background-image:url(../img/payments/unionpay.svg); }
   * }}}
  */
  def `payment-provider-unionpay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-unionpay-dark { background-image:url(../img/payments/unionpay-dark.svg); }
   * }}}
  */
  def `payment-provider-unionpay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-verifone { background-image:url(../img/payments/verifone.svg); }
   * }}}
  */
  def `payment-provider-verifone`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-verifone-dark { background-image:url(../img/payments/verifone-dark.svg); }
   * }}}
  */
  def `payment-provider-verifone-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-verisign { background-image:url(../img/payments/verisign.svg); }
   * }}}
  */
  def `payment-provider-verisign`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-verisign-dark { background-image:url(../img/payments/verisign-dark.svg); }
   * }}}
  */
  def `payment-provider-verisign-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-visa { background-image:url(../img/payments/visa.svg); }
   * }}}
  */
  def `payment-provider-visa`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-visa-dark { background-image:url(../img/payments/visa-dark.svg); }
   * }}}
  */
  def `payment-provider-visa-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-webmoney { background-image:url(../img/payments/webmoney.svg); }
   * }}}
  */
  def `payment-provider-webmoney`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-webmoney-dark { background-image:url(../img/payments/webmoney-dark.svg); }
   * }}}
  */
  def `payment-provider-webmoney-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-westernunion { background-image:url(../img/payments/westernunion.svg); }
   * }}}
  */
  def `payment-provider-westernunion`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-westernunion-dark { background-image:url(../img/payments/westernunion-dark.svg); }
   * }}}
  */
  def `payment-provider-westernunion-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-worldpay { background-image:url(../img/payments/worldpay.svg); }
   * }}}
  */
  def `payment-provider-worldpay`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-provider-worldpay-dark { background-image:url(../img/payments/worldpay-dark.svg); }
   * }}}
  */
  def `payment-provider-worldpay-dark`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-sm { height:2rem; }
   * }}}
  */
  def `payment-sm`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-xl { height:5rem; }
   * }}}
  */
  def `payment-xl`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-xs { height:1.25rem; }
   * }}}
  */
  def `payment-xs`: BtsClass = this

  /**
   * Files:tabler-payments.min.css;tabler-payments.rtl.min.css
   * {{{
   * .payment-xxs { height:1rem; }
   * }}}
  */
  def `payment-xxs`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-0 { padding-bottom:0 !important; }
   * }}}
  */
  def `pb-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-1 { padding-bottom:.25rem !important; }
   * }}}
  */
  def `pb-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-2 { padding-bottom:.5rem !important; }
   * }}}
  */
  def `pb-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-3 { padding-bottom:1rem !important; }
   * }}}
  */
  def `pb-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-4 { padding-bottom:1.5rem !important; }
   * }}}
  */
  def `pb-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-5 { padding-bottom:2rem !important; }
   * }}}
  */
  def `pb-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-6 { padding-bottom:3rem !important; }
   * }}}
  */
  def `pb-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-7 { padding-bottom:5rem !important; }
   * }}}
  */
  def `pb-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-8 { padding-bottom:8rem !important; }
   * }}}
  */
  def `pb-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-0 { padding-bottom:0 !important; }
   * }}}
  */
  def `pb-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-1 { padding-bottom:.25rem !important; }
   * }}}
  */
  def `pb-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-2 { padding-bottom:.5rem !important; }
   * }}}
  */
  def `pb-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-3 { padding-bottom:1rem !important; }
   * }}}
  */
  def `pb-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-4 { padding-bottom:1.5rem !important; }
   * }}}
  */
  def `pb-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-5 { padding-bottom:2rem !important; }
   * }}}
  */
  def `pb-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-6 { padding-bottom:3rem !important; }
   * }}}
  */
  def `pb-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-7 { padding-bottom:5rem !important; }
   * }}}
  */
  def `pb-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-lg-8 { padding-bottom:8rem !important; }
   * }}}
  */
  def `pb-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-0 { padding-bottom:0 !important; }
   * }}}
  */
  def `pb-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-1 { padding-bottom:.25rem !important; }
   * }}}
  */
  def `pb-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-2 { padding-bottom:.5rem !important; }
   * }}}
  */
  def `pb-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-3 { padding-bottom:1rem !important; }
   * }}}
  */
  def `pb-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-4 { padding-bottom:1.5rem !important; }
   * }}}
  */
  def `pb-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-5 { padding-bottom:2rem !important; }
   * }}}
  */
  def `pb-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-6 { padding-bottom:3rem !important; }
   * }}}
  */
  def `pb-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-7 { padding-bottom:5rem !important; }
   * }}}
  */
  def `pb-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-md-8 { padding-bottom:8rem !important; }
   * }}}
  */
  def `pb-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-0 { padding-bottom:0 !important; }
   * }}}
  */
  def `pb-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-1 { padding-bottom:.25rem !important; }
   * }}}
  */
  def `pb-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-2 { padding-bottom:.5rem !important; }
   * }}}
  */
  def `pb-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-3 { padding-bottom:1rem !important; }
   * }}}
  */
  def `pb-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-4 { padding-bottom:1.5rem !important; }
   * }}}
  */
  def `pb-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-5 { padding-bottom:2rem !important; }
   * }}}
  */
  def `pb-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-6 { padding-bottom:3rem !important; }
   * }}}
  */
  def `pb-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-7 { padding-bottom:5rem !important; }
   * }}}
  */
  def `pb-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-sm-8 { padding-bottom:8rem !important; }
   * }}}
  */
  def `pb-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-0 { padding-bottom:0 !important; }
   * }}}
  */
  def `pb-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-1 { padding-bottom:.25rem !important; }
   * }}}
  */
  def `pb-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-2 { padding-bottom:.5rem !important; }
   * }}}
  */
  def `pb-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-3 { padding-bottom:1rem !important; }
   * }}}
  */
  def `pb-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-4 { padding-bottom:1.5rem !important; }
   * }}}
  */
  def `pb-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-5 { padding-bottom:2rem !important; }
   * }}}
  */
  def `pb-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-6 { padding-bottom:3rem !important; }
   * }}}
  */
  def `pb-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-7 { padding-bottom:5rem !important; }
   * }}}
  */
  def `pb-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xl-8 { padding-bottom:8rem !important; }
   * }}}
  */
  def `pb-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-0 { padding-bottom:0 !important; }
   * }}}
  */
  def `pb-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-1 { padding-bottom:.25rem !important; }
   * }}}
  */
  def `pb-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-2 { padding-bottom:.5rem !important; }
   * }}}
  */
  def `pb-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-3 { padding-bottom:1rem !important; }
   * }}}
  */
  def `pb-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-4 { padding-bottom:1.5rem !important; }
   * }}}
  */
  def `pb-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-5 { padding-bottom:2rem !important; }
   * }}}
  */
  def `pb-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-6 { padding-bottom:3rem !important; }
   * }}}
  */
  def `pb-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-7 { padding-bottom:5rem !important; }
   * }}}
  */
  def `pb-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pb-xxl-8 { padding-bottom:8rem !important; }
   * }}}
  */
  def `pb-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-0 { padding-right:0 !important; }
   * }}}
  */
  def `pe-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-1 { padding-right:.25rem !important; }
   * }}}
  */
  def `pe-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-2 { padding-right:.5rem !important; }
   * }}}
  */
  def `pe-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-3 { padding-right:1rem !important; }
   * }}}
  */
  def `pe-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-4 { padding-right:1.5rem !important; }
   * }}}
  */
  def `pe-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-5 { padding-right:2rem !important; }
   * }}}
  */
  def `pe-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-6 { padding-right:3rem !important; }
   * }}}
  */
  def `pe-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-7 { padding-right:5rem !important; }
   * }}}
  */
  def `pe-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-8 { padding-right:8rem !important; }
   * }}}
  */
  def `pe-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-auto { pointer-events:auto !important; }
   * }}}
  */
  def `pe-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-0 { padding-right:0 !important; }
   * }}}
  */
  def `pe-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-1 { padding-right:.25rem !important; }
   * }}}
  */
  def `pe-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-2 { padding-right:.5rem !important; }
   * }}}
  */
  def `pe-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-3 { padding-right:1rem !important; }
   * }}}
  */
  def `pe-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-4 { padding-right:1.5rem !important; }
   * }}}
  */
  def `pe-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-5 { padding-right:2rem !important; }
   * }}}
  */
  def `pe-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-6 { padding-right:3rem !important; }
   * }}}
  */
  def `pe-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-7 { padding-right:5rem !important; }
   * }}}
  */
  def `pe-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-lg-8 { padding-right:8rem !important; }
   * }}}
  */
  def `pe-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-0 { padding-right:0 !important; }
   * }}}
  */
  def `pe-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-1 { padding-right:.25rem !important; }
   * }}}
  */
  def `pe-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-2 { padding-right:.5rem !important; }
   * }}}
  */
  def `pe-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-3 { padding-right:1rem !important; }
   * }}}
  */
  def `pe-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-4 { padding-right:1.5rem !important; }
   * }}}
  */
  def `pe-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-5 { padding-right:2rem !important; }
   * }}}
  */
  def `pe-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-6 { padding-right:3rem !important; }
   * }}}
  */
  def `pe-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-7 { padding-right:5rem !important; }
   * }}}
  */
  def `pe-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-md-8 { padding-right:8rem !important; }
   * }}}
  */
  def `pe-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-none { pointer-events:none !important; }
   * }}}
  */
  def `pe-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-0 { padding-right:0 !important; }
   * }}}
  */
  def `pe-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-1 { padding-right:.25rem !important; }
   * }}}
  */
  def `pe-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-2 { padding-right:.5rem !important; }
   * }}}
  */
  def `pe-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-3 { padding-right:1rem !important; }
   * }}}
  */
  def `pe-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-4 { padding-right:1.5rem !important; }
   * }}}
  */
  def `pe-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-5 { padding-right:2rem !important; }
   * }}}
  */
  def `pe-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-6 { padding-right:3rem !important; }
   * }}}
  */
  def `pe-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-7 { padding-right:5rem !important; }
   * }}}
  */
  def `pe-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-sm-8 { padding-right:8rem !important; }
   * }}}
  */
  def `pe-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-0 { padding-right:0 !important; }
   * }}}
  */
  def `pe-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-1 { padding-right:.25rem !important; }
   * }}}
  */
  def `pe-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-2 { padding-right:.5rem !important; }
   * }}}
  */
  def `pe-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-3 { padding-right:1rem !important; }
   * }}}
  */
  def `pe-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-4 { padding-right:1.5rem !important; }
   * }}}
  */
  def `pe-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-5 { padding-right:2rem !important; }
   * }}}
  */
  def `pe-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-6 { padding-right:3rem !important; }
   * }}}
  */
  def `pe-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-7 { padding-right:5rem !important; }
   * }}}
  */
  def `pe-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xl-8 { padding-right:8rem !important; }
   * }}}
  */
  def `pe-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-0 { padding-right:0 !important; }
   * }}}
  */
  def `pe-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-1 { padding-right:.25rem !important; }
   * }}}
  */
  def `pe-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-2 { padding-right:.5rem !important; }
   * }}}
  */
  def `pe-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-3 { padding-right:1rem !important; }
   * }}}
  */
  def `pe-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-4 { padding-right:1.5rem !important; }
   * }}}
  */
  def `pe-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-5 { padding-right:2rem !important; }
   * }}}
  */
  def `pe-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-6 { padding-right:3rem !important; }
   * }}}
  */
  def `pe-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-7 { padding-right:5rem !important; }
   * }}}
  */
  def `pe-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pe-xxl-8 { padding-right:8rem !important; }
   * }}}
  */
  def `pe-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .placeholder {
   *   display:inline-block;
   *   min-height:1em;
   *   vertical-align:middle;
   *   cursor:wait;
   *   background-color:currentcolor;
   *   opacity:.2;
   * }
   * }}}
  */
  def placeholder: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .placeholder-glow .placeholder { animation:placeholder-glow 2s ease-in-out infinite; }
   * }}}
  */
  def `placeholder-glow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .placeholder-lg { min-height:1.2em; }
   * }}}
  */
  def `placeholder-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .placeholder-sm { min-height:.8em; }
   * }}}
  */
  def `placeholder-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .placeholder-wave {
   *   -webkit-mask-image:linear-gradient(130deg,#000 55%,rgba(0,0,0,.9) 75%,#000 95%);
   *   mask-image:linear-gradient(130deg,#000 55%,rgba(0,0,0,.9) 75%,#000 95%);
   *   -webkit-mask-size:200% 100%;
   *   mask-size:200% 100%;
   *   animation:placeholder-wave 2s linear infinite;
   * }
   * }}}
  */
  def `placeholder-wave`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .placeholder-xs { min-height:.6em; }
   * }}}
  */
  def `placeholder-xs`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-checkbox_options .option input { margin-right:.5rem; }
   * }}}
  */
  def `plugin-checkbox_options`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-clear_button { --ts-pr-clear-button:1em; }
   * }}}
  */
  def `plugin-clear_button`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-drag_drop.multi>.ts-control>div.ui-sortable-placeholder {
   *   visibility:visible !important;
   *   background:#f2f2f2 !important;
   *   background:rgba(0,0,0,.06) !important;
   *   border:0 none !important;
   *   box-shadow:inset 0 0 12px 4px #fff;
   * }
   * }}}
  */
  def `plugin-drag_drop`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-dropdown_input.focus.dropdown-active .ts-control {
   *   box-shadow:none;
   *   border:1px solid var(--tblr-border-color);
   *   box-shadow:var(--tblr-box-shadow-input);
   * }
   * }}}
  */
  def `plugin-dropdown_input`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-input_autogrow.has-items .ts-control>input { min-width:0; }
   * }}}
  */
  def `plugin-input_autogrow`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown.plugin-optgroup_columns .ts-dropdown-content { display:flex; }
   * }}}
  */
  def `plugin-optgroup_columns`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-remove_button .item {
   *   display:inline-flex;
   *   align-items:center;
   *   padding-right:0 !important;
   * }
   * }}}
  */
  def `plugin-remove_button`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .carousel.pointer-event { touch-action:pan-y; }
   * }}}
  */
  def `pointer-event`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .popover {
   *   --tblr-popover-zindex:1070;
   *   --tblr-popover-max-width:276px;
   *   --tblr-popover-font-size:0.765625rem;
   *   --tblr-popover-bg:var(--tblr-bg-surface);
   *   --tblr-popover-border-width:var(--tblr-border-width);
   *   --tblr-popover-border-color:var(--tblr-border-color);
   *   --tblr-popover-border-radius:var(--tblr-border-radius-lg);
   *   --tblr-popover-inner-border-radius:calc(var(--tblr-border-radius-lg) - var(--tblr-border-width));
   *   --tblr-popover-box-shadow:rgba(var(--tblr-body-color-rgb),0.04) 0 2px 4px 0;
   *   --tblr-popover-header-padding-x:1rem;
   *   --tblr-popover-header-padding-y:0.5rem;
   *   --tblr-popover-header-font-size:0.875rem;
   *   --tblr-popover-header-color:inherit;
   *   --tblr-popover-header-bg:transparent;
   *   --tblr-popover-body-padding-x:1rem;
   *   --tblr-popover-body-padding-y:1rem;
   *   --tblr-popover-body-color:inherit;
   *   --tblr-popover-arrow-width:1rem;
   *   --tblr-popover-arrow-height:0.5rem;
   *   --tblr-popover-arrow-border:var(--tblr-popover-border-color);
   *   z-index:var(--tblr-popover-zindex);
   *   display:block;
   *   max-width:var(--tblr-popover-max-width);
   *   font-family:var(--tblr-font-sans-serif);
   *   font-style:normal;
   *   font-weight:400;
   *   line-height:1.4285714286;
   *   text-align:left;
   *   text-align:start;
   *   text-decoration:none;
   *   text-shadow:none;
   *   text-transform:none;
   *   letter-spacing:normal;
   *   word-break:normal;
   *   white-space:normal;
   *   word-spacing:normal;
   *   line-break:auto;
   *   font-size:var(--tblr-popover-font-size);
   *   word-wrap:break-word;
   *   background-color:var(--tblr-popover-bg);
   *   background-clip:padding-box;
   *   border:var(--tblr-popover-border-width) solid var(--tblr-popover-border-color);
   *   border-radius:var(--tblr-popover-border-radius);
   *   box-shadow:var(--tblr-popover-box-shadow);
   * }
   * }}}
  */
  def popover: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .popover .popover-arrow {
   *   display:block;
   *   width:var(--tblr-popover-arrow-width);
   *   height:var(--tblr-popover-arrow-height);
   * }
   * }}}
  */
  def `popover-arrow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .popover-body {
   *   padding:var(--tblr-popover-body-padding-y) var(--tblr-popover-body-padding-x);
   *   color:var(--tblr-popover-body-color);
   * }
   * }}}
  */
  def `popover-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .bs-popover-auto[data-popper-placement^=bottom] .popover-header::before,
   * .bs-popover-bottom .popover-header::before {
   *   position:absolute;
   *   top:0;
   *   left:50%;
   *   display:block;
   *   width:var(--tblr-popover-arrow-width);
   *   margin-left:calc(-.5*var(--tblr-popover-arrow-width));
   *   content:"";
   *   border-bottom:var(--tblr-popover-border-width) solid var(--tblr-popover-header-bg);
   * }
   * }}}
  */
  def `popover-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .position-absolute { position:absolute !important; }
   * }}}
  */
  def `position-absolute`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .position-fixed { position:fixed !important; }
   * }}}
  */
  def `position-fixed`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .position-relative { position:relative !important; }
   * }}}
  */
  def `position-relative`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .position-static { position:static !important; }
   * }}}
  */
  def `position-static`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .position-sticky {
   *   position:-webkit-sticky !important;
   *   position:sticky !important;
   * }
   * }}}
  */
  def `position-sticky`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-date.next-month,
   * .calendar-date.prev-month { opacity:.25; }
   * }}}
  */
  def `prev-month`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress,
   * .progress-stacked {
   *   --tblr-progress-height:0.5rem;
   *   --tblr-progress-font-size:0.65625rem;
   *   --tblr-progress-bg:var(--tblr-border-color);
   *   --tblr-progress-border-radius:var(--tblr-border-radius);
   *   --tblr-progress-box-shadow:var(--tblr-box-shadow-inset);
   *   --tblr-progress-bar-color:#ffffff;
   *   --tblr-progress-bar-bg:var(--tblr-primary);
   *   --tblr-progress-bar-transition:width 0.6s ease;
   *   display:flex;
   *   height:var(--tblr-progress-height);
   *   overflow:hidden;
   *   font-size:var(--tblr-progress-font-size);
   *   background-color:var(--tblr-progress-bg);
   *   border-radius:var(--tblr-progress-border-radius);
   *   box-shadow:var(--tblr-progress-box-shadow);
   * }
   * }}}
  */
  def progress: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress-bar {
   *   display:flex;
   *   flex-direction:column;
   *   justify-content:center;
   *   overflow:hidden;
   *   color:var(--tblr-progress-bar-color);
   *   text-align:center;
   *   white-space:nowrap;
   *   background-color:var(--tblr-progress-bar-bg);
   *   transition:var(--tblr-progress-bar-transition);
   * }
   * }}}
  */
  def `progress-bar`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress-bar-animated { animation:1s linear infinite progress-bar-stripes; }
   * }}}
  */
  def `progress-bar-animated`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress-bar-indeterminate:after,
   * .progress-bar-indeterminate:before {
   *   position:absolute;
   *   top:0;
   *   bottom:0;
   *   left:0;
   *   content:"";
   *   background-color:inherit;
   *   will-change:left,right;
   * }
   * }}}
  */
  def `progress-bar-indeterminate`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress-bar-striped {
   *   background-image:linear-gradient(45deg,rgba(255,255,255,.15) 25%,transparent 25%,transparent 50%,rgba(255,255,255,.15) 50%,rgba(255,255,255,.15) 75%,transparent 75%,transparent);
   *   background-size:var(--tblr-progress-height) var(--tblr-progress-height);
   * }
   * }}}
  */
  def `progress-bar-striped`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress-separated .progress-bar { box-shadow:0 0 0 2px var(--tblr-card-bg,var(--tblr-bg-surface)); }
   * }}}
  */
  def `progress-separated`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress-sm { height:.25rem; }
   * }}}
  */
  def `progress-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progress,
   * .progress-stacked {
   *   --tblr-progress-height:0.5rem;
   *   --tblr-progress-font-size:0.65625rem;
   *   --tblr-progress-bg:var(--tblr-border-color);
   *   --tblr-progress-border-radius:var(--tblr-border-radius);
   *   --tblr-progress-box-shadow:var(--tblr-box-shadow-inset);
   *   --tblr-progress-bar-color:#ffffff;
   *   --tblr-progress-bar-bg:var(--tblr-primary);
   *   --tblr-progress-bar-transition:width 0.6s ease;
   *   display:flex;
   *   height:var(--tblr-progress-height);
   *   overflow:hidden;
   *   font-size:var(--tblr-progress-font-size);
   *   background-color:var(--tblr-progress-bg);
   *   border-radius:var(--tblr-progress-border-radius);
   *   box-shadow:var(--tblr-progress-box-shadow);
   * }
   * }}}
  */
  def `progress-stacked`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progressbg {
   *   position:relative;
   *   padding:.25rem .5rem;
   *   display:flex;
   * }
   * }}}
  */
  def progressbg: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progressbg-progress {
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   bottom:0;
   *   left:0;
   *   z-index:0;
   *   height:100%;
   *   background:0 0;
   *   pointer-events:none;
   * }
   * }}}
  */
  def `progressbg-progress`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progressbg-text {
   *   position:relative;
   *   z-index:1;
   *   overflow:hidden;
   *   text-overflow:ellipsis;
   *   white-space:nowrap;
   * }
   * }}}
  */
  def `progressbg-text`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .progressbg-value {
   *   font-weight:var(--tblr-font-weight-medium);
   *   margin-left:auto;
   *   padding-left:2rem;
   * }
   * }}}
  */
  def `progressbg-value`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-0 { padding-left:0 !important; }
   * }}}
  */
  def `ps-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-1 { padding-left:.25rem !important; }
   * }}}
  */
  def `ps-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-2 { padding-left:.5rem !important; }
   * }}}
  */
  def `ps-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-3 { padding-left:1rem !important; }
   * }}}
  */
  def `ps-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-4 { padding-left:1.5rem !important; }
   * }}}
  */
  def `ps-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-5 { padding-left:2rem !important; }
   * }}}
  */
  def `ps-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-6 { padding-left:3rem !important; }
   * }}}
  */
  def `ps-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-7 { padding-left:5rem !important; }
   * }}}
  */
  def `ps-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-8 { padding-left:8rem !important; }
   * }}}
  */
  def `ps-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-0 { padding-left:0 !important; }
   * }}}
  */
  def `ps-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-1 { padding-left:.25rem !important; }
   * }}}
  */
  def `ps-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-2 { padding-left:.5rem !important; }
   * }}}
  */
  def `ps-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-3 { padding-left:1rem !important; }
   * }}}
  */
  def `ps-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-4 { padding-left:1.5rem !important; }
   * }}}
  */
  def `ps-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-5 { padding-left:2rem !important; }
   * }}}
  */
  def `ps-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-6 { padding-left:3rem !important; }
   * }}}
  */
  def `ps-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-7 { padding-left:5rem !important; }
   * }}}
  */
  def `ps-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-lg-8 { padding-left:8rem !important; }
   * }}}
  */
  def `ps-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-0 { padding-left:0 !important; }
   * }}}
  */
  def `ps-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-1 { padding-left:.25rem !important; }
   * }}}
  */
  def `ps-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-2 { padding-left:.5rem !important; }
   * }}}
  */
  def `ps-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-3 { padding-left:1rem !important; }
   * }}}
  */
  def `ps-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-4 { padding-left:1.5rem !important; }
   * }}}
  */
  def `ps-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-5 { padding-left:2rem !important; }
   * }}}
  */
  def `ps-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-6 { padding-left:3rem !important; }
   * }}}
  */
  def `ps-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-7 { padding-left:5rem !important; }
   * }}}
  */
  def `ps-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-md-8 { padding-left:8rem !important; }
   * }}}
  */
  def `ps-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-0 { padding-left:0 !important; }
   * }}}
  */
  def `ps-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-1 { padding-left:.25rem !important; }
   * }}}
  */
  def `ps-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-2 { padding-left:.5rem !important; }
   * }}}
  */
  def `ps-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-3 { padding-left:1rem !important; }
   * }}}
  */
  def `ps-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-4 { padding-left:1.5rem !important; }
   * }}}
  */
  def `ps-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-5 { padding-left:2rem !important; }
   * }}}
  */
  def `ps-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-6 { padding-left:3rem !important; }
   * }}}
  */
  def `ps-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-7 { padding-left:5rem !important; }
   * }}}
  */
  def `ps-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-sm-8 { padding-left:8rem !important; }
   * }}}
  */
  def `ps-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-0 { padding-left:0 !important; }
   * }}}
  */
  def `ps-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-1 { padding-left:.25rem !important; }
   * }}}
  */
  def `ps-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-2 { padding-left:.5rem !important; }
   * }}}
  */
  def `ps-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-3 { padding-left:1rem !important; }
   * }}}
  */
  def `ps-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-4 { padding-left:1.5rem !important; }
   * }}}
  */
  def `ps-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-5 { padding-left:2rem !important; }
   * }}}
  */
  def `ps-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-6 { padding-left:3rem !important; }
   * }}}
  */
  def `ps-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-7 { padding-left:5rem !important; }
   * }}}
  */
  def `ps-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xl-8 { padding-left:8rem !important; }
   * }}}
  */
  def `ps-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-0 { padding-left:0 !important; }
   * }}}
  */
  def `ps-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-1 { padding-left:.25rem !important; }
   * }}}
  */
  def `ps-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-2 { padding-left:.5rem !important; }
   * }}}
  */
  def `ps-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-3 { padding-left:1rem !important; }
   * }}}
  */
  def `ps-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-4 { padding-left:1.5rem !important; }
   * }}}
  */
  def `ps-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-5 { padding-left:2rem !important; }
   * }}}
  */
  def `ps-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-6 { padding-left:3rem !important; }
   * }}}
  */
  def `ps-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-7 { padding-left:5rem !important; }
   * }}}
  */
  def `ps-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ps-xxl-8 { padding-left:8rem !important; }
   * }}}
  */
  def `ps-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-0 { padding-top:0 !important; }
   * }}}
  */
  def `pt-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-1 { padding-top:.25rem !important; }
   * }}}
  */
  def `pt-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-2 { padding-top:.5rem !important; }
   * }}}
  */
  def `pt-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-3 { padding-top:1rem !important; }
   * }}}
  */
  def `pt-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-4 { padding-top:1.5rem !important; }
   * }}}
  */
  def `pt-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-5 { padding-top:2rem !important; }
   * }}}
  */
  def `pt-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-6 { padding-top:3rem !important; }
   * }}}
  */
  def `pt-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-7 { padding-top:5rem !important; }
   * }}}
  */
  def `pt-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-8 { padding-top:8rem !important; }
   * }}}
  */
  def `pt-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-0 { padding-top:0 !important; }
   * }}}
  */
  def `pt-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-1 { padding-top:.25rem !important; }
   * }}}
  */
  def `pt-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-2 { padding-top:.5rem !important; }
   * }}}
  */
  def `pt-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-3 { padding-top:1rem !important; }
   * }}}
  */
  def `pt-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-4 { padding-top:1.5rem !important; }
   * }}}
  */
  def `pt-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-5 { padding-top:2rem !important; }
   * }}}
  */
  def `pt-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-6 { padding-top:3rem !important; }
   * }}}
  */
  def `pt-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-7 { padding-top:5rem !important; }
   * }}}
  */
  def `pt-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-lg-8 { padding-top:8rem !important; }
   * }}}
  */
  def `pt-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-0 { padding-top:0 !important; }
   * }}}
  */
  def `pt-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-1 { padding-top:.25rem !important; }
   * }}}
  */
  def `pt-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-2 { padding-top:.5rem !important; }
   * }}}
  */
  def `pt-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-3 { padding-top:1rem !important; }
   * }}}
  */
  def `pt-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-4 { padding-top:1.5rem !important; }
   * }}}
  */
  def `pt-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-5 { padding-top:2rem !important; }
   * }}}
  */
  def `pt-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-6 { padding-top:3rem !important; }
   * }}}
  */
  def `pt-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-7 { padding-top:5rem !important; }
   * }}}
  */
  def `pt-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-md-8 { padding-top:8rem !important; }
   * }}}
  */
  def `pt-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-0 { padding-top:0 !important; }
   * }}}
  */
  def `pt-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-1 { padding-top:.25rem !important; }
   * }}}
  */
  def `pt-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-2 { padding-top:.5rem !important; }
   * }}}
  */
  def `pt-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-3 { padding-top:1rem !important; }
   * }}}
  */
  def `pt-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-4 { padding-top:1.5rem !important; }
   * }}}
  */
  def `pt-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-5 { padding-top:2rem !important; }
   * }}}
  */
  def `pt-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-6 { padding-top:3rem !important; }
   * }}}
  */
  def `pt-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-7 { padding-top:5rem !important; }
   * }}}
  */
  def `pt-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-sm-8 { padding-top:8rem !important; }
   * }}}
  */
  def `pt-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-0 { padding-top:0 !important; }
   * }}}
  */
  def `pt-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-1 { padding-top:.25rem !important; }
   * }}}
  */
  def `pt-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-2 { padding-top:.5rem !important; }
   * }}}
  */
  def `pt-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-3 { padding-top:1rem !important; }
   * }}}
  */
  def `pt-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-4 { padding-top:1.5rem !important; }
   * }}}
  */
  def `pt-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-5 { padding-top:2rem !important; }
   * }}}
  */
  def `pt-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-6 { padding-top:3rem !important; }
   * }}}
  */
  def `pt-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-7 { padding-top:5rem !important; }
   * }}}
  */
  def `pt-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xl-8 { padding-top:8rem !important; }
   * }}}
  */
  def `pt-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-0 { padding-top:0 !important; }
   * }}}
  */
  def `pt-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-1 { padding-top:.25rem !important; }
   * }}}
  */
  def `pt-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-2 { padding-top:.5rem !important; }
   * }}}
  */
  def `pt-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-3 { padding-top:1rem !important; }
   * }}}
  */
  def `pt-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-4 { padding-top:1.5rem !important; }
   * }}}
  */
  def `pt-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-5 { padding-top:2rem !important; }
   * }}}
  */
  def `pt-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-6 { padding-top:3rem !important; }
   * }}}
  */
  def `pt-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-7 { padding-top:5rem !important; }
   * }}}
  */
  def `pt-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .pt-xxl-8 { padding-top:8rem !important; }
   * }}}
  */
  def `pt-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-0 {
   *   padding-right:0 !important;
   *   padding-left:0 !important;
   * }
   * }}}
  */
  def `px-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-1 {
   *   padding-right:.25rem !important;
   *   padding-left:.25rem !important;
   * }
   * }}}
  */
  def `px-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-2 {
   *   padding-right:.5rem !important;
   *   padding-left:.5rem !important;
   * }
   * }}}
  */
  def `px-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-3 {
   *   padding-right:1rem !important;
   *   padding-left:1rem !important;
   * }
   * }}}
  */
  def `px-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-4 {
   *   padding-right:1.5rem !important;
   *   padding-left:1.5rem !important;
   * }
   * }}}
  */
  def `px-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-5 {
   *   padding-right:2rem !important;
   *   padding-left:2rem !important;
   * }
   * }}}
  */
  def `px-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-6 {
   *   padding-right:3rem !important;
   *   padding-left:3rem !important;
   * }
   * }}}
  */
  def `px-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-7 {
   *   padding-right:5rem !important;
   *   padding-left:5rem !important;
   * }
   * }}}
  */
  def `px-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-8 {
   *   padding-right:8rem !important;
   *   padding-left:8rem !important;
   * }
   * }}}
  */
  def `px-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-0 {
   *   padding-right:0 !important;
   *   padding-left:0 !important;
   * }
   * }}}
  */
  def `px-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-1 {
   *   padding-right:.25rem !important;
   *   padding-left:.25rem !important;
   * }
   * }}}
  */
  def `px-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-2 {
   *   padding-right:.5rem !important;
   *   padding-left:.5rem !important;
   * }
   * }}}
  */
  def `px-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-3 {
   *   padding-right:1rem !important;
   *   padding-left:1rem !important;
   * }
   * }}}
  */
  def `px-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-4 {
   *   padding-right:1.5rem !important;
   *   padding-left:1.5rem !important;
   * }
   * }}}
  */
  def `px-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-5 {
   *   padding-right:2rem !important;
   *   padding-left:2rem !important;
   * }
   * }}}
  */
  def `px-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-6 {
   *   padding-right:3rem !important;
   *   padding-left:3rem !important;
   * }
   * }}}
  */
  def `px-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-7 {
   *   padding-right:5rem !important;
   *   padding-left:5rem !important;
   * }
   * }}}
  */
  def `px-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-lg-8 {
   *   padding-right:8rem !important;
   *   padding-left:8rem !important;
   * }
   * }}}
  */
  def `px-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-0 {
   *   padding-right:0 !important;
   *   padding-left:0 !important;
   * }
   * }}}
  */
  def `px-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-1 {
   *   padding-right:.25rem !important;
   *   padding-left:.25rem !important;
   * }
   * }}}
  */
  def `px-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-2 {
   *   padding-right:.5rem !important;
   *   padding-left:.5rem !important;
   * }
   * }}}
  */
  def `px-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-3 {
   *   padding-right:1rem !important;
   *   padding-left:1rem !important;
   * }
   * }}}
  */
  def `px-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-4 {
   *   padding-right:1.5rem !important;
   *   padding-left:1.5rem !important;
   * }
   * }}}
  */
  def `px-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-5 {
   *   padding-right:2rem !important;
   *   padding-left:2rem !important;
   * }
   * }}}
  */
  def `px-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-6 {
   *   padding-right:3rem !important;
   *   padding-left:3rem !important;
   * }
   * }}}
  */
  def `px-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-7 {
   *   padding-right:5rem !important;
   *   padding-left:5rem !important;
   * }
   * }}}
  */
  def `px-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-md-8 {
   *   padding-right:8rem !important;
   *   padding-left:8rem !important;
   * }
   * }}}
  */
  def `px-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-0 {
   *   padding-right:0 !important;
   *   padding-left:0 !important;
   * }
   * }}}
  */
  def `px-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-1 {
   *   padding-right:.25rem !important;
   *   padding-left:.25rem !important;
   * }
   * }}}
  */
  def `px-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-2 {
   *   padding-right:.5rem !important;
   *   padding-left:.5rem !important;
   * }
   * }}}
  */
  def `px-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-3 {
   *   padding-right:1rem !important;
   *   padding-left:1rem !important;
   * }
   * }}}
  */
  def `px-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-4 {
   *   padding-right:1.5rem !important;
   *   padding-left:1.5rem !important;
   * }
   * }}}
  */
  def `px-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-5 {
   *   padding-right:2rem !important;
   *   padding-left:2rem !important;
   * }
   * }}}
  */
  def `px-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-6 {
   *   padding-right:3rem !important;
   *   padding-left:3rem !important;
   * }
   * }}}
  */
  def `px-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-7 {
   *   padding-right:5rem !important;
   *   padding-left:5rem !important;
   * }
   * }}}
  */
  def `px-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-sm-8 {
   *   padding-right:8rem !important;
   *   padding-left:8rem !important;
   * }
   * }}}
  */
  def `px-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-0 {
   *   padding-right:0 !important;
   *   padding-left:0 !important;
   * }
   * }}}
  */
  def `px-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-1 {
   *   padding-right:.25rem !important;
   *   padding-left:.25rem !important;
   * }
   * }}}
  */
  def `px-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-2 {
   *   padding-right:.5rem !important;
   *   padding-left:.5rem !important;
   * }
   * }}}
  */
  def `px-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-3 {
   *   padding-right:1rem !important;
   *   padding-left:1rem !important;
   * }
   * }}}
  */
  def `px-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-4 {
   *   padding-right:1.5rem !important;
   *   padding-left:1.5rem !important;
   * }
   * }}}
  */
  def `px-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-5 {
   *   padding-right:2rem !important;
   *   padding-left:2rem !important;
   * }
   * }}}
  */
  def `px-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-6 {
   *   padding-right:3rem !important;
   *   padding-left:3rem !important;
   * }
   * }}}
  */
  def `px-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-7 {
   *   padding-right:5rem !important;
   *   padding-left:5rem !important;
   * }
   * }}}
  */
  def `px-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xl-8 {
   *   padding-right:8rem !important;
   *   padding-left:8rem !important;
   * }
   * }}}
  */
  def `px-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-0 {
   *   padding-right:0 !important;
   *   padding-left:0 !important;
   * }
   * }}}
  */
  def `px-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-1 {
   *   padding-right:.25rem !important;
   *   padding-left:.25rem !important;
   * }
   * }}}
  */
  def `px-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-2 {
   *   padding-right:.5rem !important;
   *   padding-left:.5rem !important;
   * }
   * }}}
  */
  def `px-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-3 {
   *   padding-right:1rem !important;
   *   padding-left:1rem !important;
   * }
   * }}}
  */
  def `px-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-4 {
   *   padding-right:1.5rem !important;
   *   padding-left:1.5rem !important;
   * }
   * }}}
  */
  def `px-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-5 {
   *   padding-right:2rem !important;
   *   padding-left:2rem !important;
   * }
   * }}}
  */
  def `px-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-6 {
   *   padding-right:3rem !important;
   *   padding-left:3rem !important;
   * }
   * }}}
  */
  def `px-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-7 {
   *   padding-right:5rem !important;
   *   padding-left:5rem !important;
   * }
   * }}}
  */
  def `px-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .px-xxl-8 {
   *   padding-right:8rem !important;
   *   padding-left:8rem !important;
   * }
   * }}}
  */
  def `px-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-0 {
   *   padding-top:0 !important;
   *   padding-bottom:0 !important;
   * }
   * }}}
  */
  def `py-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-1 {
   *   padding-top:.25rem !important;
   *   padding-bottom:.25rem !important;
   * }
   * }}}
  */
  def `py-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-2 {
   *   padding-top:.5rem !important;
   *   padding-bottom:.5rem !important;
   * }
   * }}}
  */
  def `py-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-3 {
   *   padding-top:1rem !important;
   *   padding-bottom:1rem !important;
   * }
   * }}}
  */
  def `py-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-4 {
   *   padding-top:1.5rem !important;
   *   padding-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `py-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-5 {
   *   padding-top:2rem !important;
   *   padding-bottom:2rem !important;
   * }
   * }}}
  */
  def `py-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-6 {
   *   padding-top:3rem !important;
   *   padding-bottom:3rem !important;
   * }
   * }}}
  */
  def `py-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-7 {
   *   padding-top:5rem !important;
   *   padding-bottom:5rem !important;
   * }
   * }}}
  */
  def `py-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-8 {
   *   padding-top:8rem !important;
   *   padding-bottom:8rem !important;
   * }
   * }}}
  */
  def `py-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-0 {
   *   padding-top:0 !important;
   *   padding-bottom:0 !important;
   * }
   * }}}
  */
  def `py-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-1 {
   *   padding-top:.25rem !important;
   *   padding-bottom:.25rem !important;
   * }
   * }}}
  */
  def `py-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-2 {
   *   padding-top:.5rem !important;
   *   padding-bottom:.5rem !important;
   * }
   * }}}
  */
  def `py-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-3 {
   *   padding-top:1rem !important;
   *   padding-bottom:1rem !important;
   * }
   * }}}
  */
  def `py-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-4 {
   *   padding-top:1.5rem !important;
   *   padding-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `py-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-5 {
   *   padding-top:2rem !important;
   *   padding-bottom:2rem !important;
   * }
   * }}}
  */
  def `py-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-6 {
   *   padding-top:3rem !important;
   *   padding-bottom:3rem !important;
   * }
   * }}}
  */
  def `py-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-7 {
   *   padding-top:5rem !important;
   *   padding-bottom:5rem !important;
   * }
   * }}}
  */
  def `py-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-lg-8 {
   *   padding-top:8rem !important;
   *   padding-bottom:8rem !important;
   * }
   * }}}
  */
  def `py-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-0 {
   *   padding-top:0 !important;
   *   padding-bottom:0 !important;
   * }
   * }}}
  */
  def `py-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-1 {
   *   padding-top:.25rem !important;
   *   padding-bottom:.25rem !important;
   * }
   * }}}
  */
  def `py-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-2 {
   *   padding-top:.5rem !important;
   *   padding-bottom:.5rem !important;
   * }
   * }}}
  */
  def `py-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-3 {
   *   padding-top:1rem !important;
   *   padding-bottom:1rem !important;
   * }
   * }}}
  */
  def `py-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-4 {
   *   padding-top:1.5rem !important;
   *   padding-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `py-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-5 {
   *   padding-top:2rem !important;
   *   padding-bottom:2rem !important;
   * }
   * }}}
  */
  def `py-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-6 {
   *   padding-top:3rem !important;
   *   padding-bottom:3rem !important;
   * }
   * }}}
  */
  def `py-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-7 {
   *   padding-top:5rem !important;
   *   padding-bottom:5rem !important;
   * }
   * }}}
  */
  def `py-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-md-8 {
   *   padding-top:8rem !important;
   *   padding-bottom:8rem !important;
   * }
   * }}}
  */
  def `py-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-0 {
   *   padding-top:0 !important;
   *   padding-bottom:0 !important;
   * }
   * }}}
  */
  def `py-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-1 {
   *   padding-top:.25rem !important;
   *   padding-bottom:.25rem !important;
   * }
   * }}}
  */
  def `py-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-2 {
   *   padding-top:.5rem !important;
   *   padding-bottom:.5rem !important;
   * }
   * }}}
  */
  def `py-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-3 {
   *   padding-top:1rem !important;
   *   padding-bottom:1rem !important;
   * }
   * }}}
  */
  def `py-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-4 {
   *   padding-top:1.5rem !important;
   *   padding-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `py-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-5 {
   *   padding-top:2rem !important;
   *   padding-bottom:2rem !important;
   * }
   * }}}
  */
  def `py-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-6 {
   *   padding-top:3rem !important;
   *   padding-bottom:3rem !important;
   * }
   * }}}
  */
  def `py-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-7 {
   *   padding-top:5rem !important;
   *   padding-bottom:5rem !important;
   * }
   * }}}
  */
  def `py-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-sm-8 {
   *   padding-top:8rem !important;
   *   padding-bottom:8rem !important;
   * }
   * }}}
  */
  def `py-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-0 {
   *   padding-top:0 !important;
   *   padding-bottom:0 !important;
   * }
   * }}}
  */
  def `py-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-1 {
   *   padding-top:.25rem !important;
   *   padding-bottom:.25rem !important;
   * }
   * }}}
  */
  def `py-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-2 {
   *   padding-top:.5rem !important;
   *   padding-bottom:.5rem !important;
   * }
   * }}}
  */
  def `py-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-3 {
   *   padding-top:1rem !important;
   *   padding-bottom:1rem !important;
   * }
   * }}}
  */
  def `py-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-4 {
   *   padding-top:1.5rem !important;
   *   padding-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `py-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-5 {
   *   padding-top:2rem !important;
   *   padding-bottom:2rem !important;
   * }
   * }}}
  */
  def `py-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-6 {
   *   padding-top:3rem !important;
   *   padding-bottom:3rem !important;
   * }
   * }}}
  */
  def `py-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-7 {
   *   padding-top:5rem !important;
   *   padding-bottom:5rem !important;
   * }
   * }}}
  */
  def `py-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xl-8 {
   *   padding-top:8rem !important;
   *   padding-bottom:8rem !important;
   * }
   * }}}
  */
  def `py-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-0 {
   *   padding-top:0 !important;
   *   padding-bottom:0 !important;
   * }
   * }}}
  */
  def `py-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-1 {
   *   padding-top:.25rem !important;
   *   padding-bottom:.25rem !important;
   * }
   * }}}
  */
  def `py-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-2 {
   *   padding-top:.5rem !important;
   *   padding-bottom:.5rem !important;
   * }
   * }}}
  */
  def `py-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-3 {
   *   padding-top:1rem !important;
   *   padding-bottom:1rem !important;
   * }
   * }}}
  */
  def `py-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-4 {
   *   padding-top:1.5rem !important;
   *   padding-bottom:1.5rem !important;
   * }
   * }}}
  */
  def `py-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-5 {
   *   padding-top:2rem !important;
   *   padding-bottom:2rem !important;
   * }
   * }}}
  */
  def `py-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-6 {
   *   padding-top:3rem !important;
   *   padding-bottom:3rem !important;
   * }
   * }}}
  */
  def `py-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-7 {
   *   padding-top:5rem !important;
   *   padding-bottom:5rem !important;
   * }
   * }}}
  */
  def `py-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .py-xxl-8 {
   *   padding-top:8rem !important;
   *   padding-bottom:8rem !important;
   * }
   * }}}
  */
  def `py-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-range.range-end .date-item,
   * .calendar-range.range-start .date-item {
   *   color:#fff;
   *   background:var(--tblr-primary);
   *   border-color:var(--tblr-primary);
   * }
   * }}}
  */
  def `range-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .calendar-range.range-end .date-item,
   * .calendar-range.range-start .date-item {
   *   color:#fff;
   *   background:var(--tblr-primary);
   *   border-color:var(--tblr-primary);
   * }
   * }}}
  */
  def `range-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio {
   *   position:relative;
   *   width:100%;
   * }
   * }}}
  */
  def ratio: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-16x9 { --tblr-aspect-ratio:56.25%; }
   * }}}
  */
  def `ratio-16x9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-1x1 { --tblr-aspect-ratio:100%; }
   * }}}
  */
  def `ratio-1x1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-1x2 { --tblr-aspect-ratio:200%; }
   * }}}
  */
  def `ratio-1x2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-1x3 { --tblr-aspect-ratio:300%; }
   * }}}
  */
  def `ratio-1x3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-21x9 { --tblr-aspect-ratio:42.8571428571%; }
   * }}}
  */
  def `ratio-21x9`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-2x1 { --tblr-aspect-ratio:50%; }
   * }}}
  */
  def `ratio-2x1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-3x1 { --tblr-aspect-ratio:33.3333333333%; }
   * }}}
  */
  def `ratio-3x1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-3x4 { --tblr-aspect-ratio:133.3333333333%; }
   * }}}
  */
  def `ratio-3x4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-4x3 { --tblr-aspect-ratio:75%; }
   * }}}
  */
  def `ratio-4x3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-9x16 { --tblr-aspect-ratio:177.7777777778%; }
   * }}}
  */
  def `ratio-9x16`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ratio-9x21 { --tblr-aspect-ratio:233.3333333333%; }
   * }}}
  */
  def `ratio-9x21`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-remove_button .item .remove {
   *   color:inherit;
   *   text-decoration:none;
   *   vertical-align:middle;
   *   display:inline-block;
   *   padding:0 5px;
   *   border-radius:0 2px 2px 0;
   *   box-sizing:border-box;
   * }
   * }}}
  */
  def remove: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-remove_button .remove-single {
   *   position:absolute;
   *   right:0;
   *   top:0;
   *   font-size:23px;
   * }
   * }}}
  */
  def `remove-single`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .col-form-label.required:after,
   * .form-label.required:after {
   *   content:"*";
   *   margin-left:.25rem;
   *   color:#d63939;
   * }
   * }}}
  */
  def required: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon {
   *   --tblr-ribbon-margin:0.25rem;
   *   --tblr-ribbon-border-radius:var(--tblr-border-radius);
   *   position:absolute;
   *   top:.75rem;
   *   right:calc(-1*var(--tblr-ribbon-margin));
   *   z-index:1;
   *   padding:.25rem .75rem;
   *   font-size:.625rem;
   *   font-weight:var(--tblr-font-weight-bold);
   *   line-height:1;
   *   color:#fff;
   *   text-align:center;
   *   text-transform:uppercase;
   *   background:var(--tblr-primary);
   *   border-color:var(--tblr-primary);
   *   border-radius:var(--tblr-ribbon-border-radius) 0 var(--tblr-ribbon-border-radius) var(--tblr-ribbon-border-radius);
   *   display:inline-flex;
   *   align-items:center;
   *   justify-content:center;
   *   min-height:2rem;
   *   min-width:2rem;
   * }
   * }}}
  */
  def ribbon: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon-bookmark {
   *   padding-left:.25rem;
   *   border-radius:0 0 var(--tblr-ribbon-border-radius) 0;
   * }
   * }}}
  */
  def `ribbon-bookmark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon-bottom {
   *   top:auto;
   *   bottom:.75rem;
   * }
   * }}}
  */
  def `ribbon-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon-bookmark.ribbon-left { padding-right:.5rem; }
   * }}}
  */
  def `ribbon-left`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon-top.ribbon-start {
   *   right:auto;
   *   left:.75rem;
   * }
   * }}}
  */
  def `ribbon-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ribbon-top {
   *   top:calc(-1*var(--tblr-ribbon-margin));
   *   right:.75rem;
   *   width:2rem;
   *   padding:.5rem 0;
   *   border-radius:0 var(--tblr-ribbon-border-radius) var(--tblr-ribbon-border-radius) var(--tblr-ribbon-border-radius);
   * }
   * }}}
  */
  def `ribbon-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded { border-radius:var(--tblr-border-radius) !important; }
   * }}}
  */
  def rounded: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-0 { border-radius:0 !important; }
   * }}}
  */
  def `rounded-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-1 { border-radius:var(--tblr-border-radius-sm) !important; }
   * }}}
  */
  def `rounded-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-2 { border-radius:var(--tblr-border-radius) !important; }
   * }}}
  */
  def `rounded-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-3 { border-radius:var(--tblr-border-radius-lg) !important; }
   * }}}
  */
  def `rounded-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-4 { border-radius:var(--tblr-border-radius-xl) !important; }
   * }}}
  */
  def `rounded-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-5 { border-radius:var(--tblr-border-radius-xxl) !important; }
   * }}}
  */
  def `rounded-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom {
   *   border-bottom-right-radius:var(--tblr-border-radius) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-0 {
   *   border-bottom-right-radius:0 !important;
   *   border-bottom-left-radius:0 !important;
   * }
   * }}}
  */
  def `rounded-bottom-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-1 {
   *   border-bottom-right-radius:var(--tblr-border-radius-sm) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius-sm) !important;
   * }
   * }}}
  */
  def `rounded-bottom-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-2 {
   *   border-bottom-right-radius:var(--tblr-border-radius) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-bottom-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-3 {
   *   border-bottom-right-radius:var(--tblr-border-radius-lg) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius-lg) !important;
   * }
   * }}}
  */
  def `rounded-bottom-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-4 {
   *   border-bottom-right-radius:var(--tblr-border-radius-xl) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius-xl) !important;
   * }
   * }}}
  */
  def `rounded-bottom-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-5 {
   *   border-bottom-right-radius:var(--tblr-border-radius-xxl) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius-xxl) !important;
   * }
   * }}}
  */
  def `rounded-bottom-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-circle {
   *   border-bottom-right-radius:50% !important;
   *   border-bottom-left-radius:50% !important;
   * }
   * }}}
  */
  def `rounded-bottom-circle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-bottom-pill {
   *   border-bottom-right-radius:var(--tblr-border-radius-pill) !important;
   *   border-bottom-left-radius:var(--tblr-border-radius-pill) !important;
   * }
   * }}}
  */
  def `rounded-bottom-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-circle { border-radius:50% !important; }
   * }}}
  */
  def `rounded-circle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end {
   *   border-top-right-radius:var(--tblr-border-radius) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-0 {
   *   border-top-right-radius:0 !important;
   *   border-bottom-right-radius:0 !important;
   * }
   * }}}
  */
  def `rounded-end-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-1 {
   *   border-top-right-radius:var(--tblr-border-radius-sm) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius-sm) !important;
   * }
   * }}}
  */
  def `rounded-end-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-2 {
   *   border-top-right-radius:var(--tblr-border-radius) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-end-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-3 {
   *   border-top-right-radius:var(--tblr-border-radius-lg) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius-lg) !important;
   * }
   * }}}
  */
  def `rounded-end-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-4 {
   *   border-top-right-radius:var(--tblr-border-radius-xl) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius-xl) !important;
   * }
   * }}}
  */
  def `rounded-end-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-5 {
   *   border-top-right-radius:var(--tblr-border-radius-xxl) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius-xxl) !important;
   * }
   * }}}
  */
  def `rounded-end-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-circle {
   *   border-top-right-radius:50% !important;
   *   border-bottom-right-radius:50% !important;
   * }
   * }}}
  */
  def `rounded-end-circle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-end-pill {
   *   border-top-right-radius:var(--tblr-border-radius-pill) !important;
   *   border-bottom-right-radius:var(--tblr-border-radius-pill) !important;
   * }
   * }}}
  */
  def `rounded-end-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-pill { border-radius:var(--tblr-border-radius-pill) !important; }
   * }}}
  */
  def `rounded-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start {
   *   border-bottom-left-radius:var(--tblr-border-radius) !important;
   *   border-top-left-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-0 {
   *   border-bottom-left-radius:0 !important;
   *   border-top-left-radius:0 !important;
   * }
   * }}}
  */
  def `rounded-start-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-1 {
   *   border-bottom-left-radius:var(--tblr-border-radius-sm) !important;
   *   border-top-left-radius:var(--tblr-border-radius-sm) !important;
   * }
   * }}}
  */
  def `rounded-start-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-2 {
   *   border-bottom-left-radius:var(--tblr-border-radius) !important;
   *   border-top-left-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-start-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-3 {
   *   border-bottom-left-radius:var(--tblr-border-radius-lg) !important;
   *   border-top-left-radius:var(--tblr-border-radius-lg) !important;
   * }
   * }}}
  */
  def `rounded-start-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-4 {
   *   border-bottom-left-radius:var(--tblr-border-radius-xl) !important;
   *   border-top-left-radius:var(--tblr-border-radius-xl) !important;
   * }
   * }}}
  */
  def `rounded-start-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-5 {
   *   border-bottom-left-radius:var(--tblr-border-radius-xxl) !important;
   *   border-top-left-radius:var(--tblr-border-radius-xxl) !important;
   * }
   * }}}
  */
  def `rounded-start-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-circle {
   *   border-bottom-left-radius:50% !important;
   *   border-top-left-radius:50% !important;
   * }
   * }}}
  */
  def `rounded-start-circle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-start-pill {
   *   border-bottom-left-radius:var(--tblr-border-radius-pill) !important;
   *   border-top-left-radius:var(--tblr-border-radius-pill) !important;
   * }
   * }}}
  */
  def `rounded-start-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top {
   *   border-top-left-radius:var(--tblr-border-radius) !important;
   *   border-top-right-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-0 {
   *   border-top-left-radius:0 !important;
   *   border-top-right-radius:0 !important;
   * }
   * }}}
  */
  def `rounded-top-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-1 {
   *   border-top-left-radius:var(--tblr-border-radius-sm) !important;
   *   border-top-right-radius:var(--tblr-border-radius-sm) !important;
   * }
   * }}}
  */
  def `rounded-top-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-2 {
   *   border-top-left-radius:var(--tblr-border-radius) !important;
   *   border-top-right-radius:var(--tblr-border-radius) !important;
   * }
   * }}}
  */
  def `rounded-top-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-3 {
   *   border-top-left-radius:var(--tblr-border-radius-lg) !important;
   *   border-top-right-radius:var(--tblr-border-radius-lg) !important;
   * }
   * }}}
  */
  def `rounded-top-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-4 {
   *   border-top-left-radius:var(--tblr-border-radius-xl) !important;
   *   border-top-right-radius:var(--tblr-border-radius-xl) !important;
   * }
   * }}}
  */
  def `rounded-top-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-5 {
   *   border-top-left-radius:var(--tblr-border-radius-xxl) !important;
   *   border-top-right-radius:var(--tblr-border-radius-xxl) !important;
   * }
   * }}}
  */
  def `rounded-top-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-circle {
   *   border-top-left-radius:50% !important;
   *   border-top-right-radius:50% !important;
   * }
   * }}}
  */
  def `rounded-top-circle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .rounded-top-pill {
   *   border-top-left-radius:var(--tblr-border-radius-pill) !important;
   *   border-top-right-radius:var(--tblr-border-radius-pill) !important;
   * }
   * }}}
  */
  def `rounded-top-pill`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row {
   *   --tblr-gutter-x:var(--tblr-page-padding);
   *   --tblr-gutter-y:0;
   *   display:flex;
   *   flex-wrap:wrap;
   *   margin-top:calc(-1*var(--tblr-gutter-y));
   *   margin-right:calc(-.5*var(--tblr-gutter-x));
   *   margin-left:calc(-.5*var(--tblr-gutter-x));
   * }
   * }}}
  */
  def row: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-0 {
   *   margin-right:0;
   *   margin-left:0;
   * }
   * }}}
  */
  def `row-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cards {
   *   --tblr-gutter-x:var(--tblr-page-padding);
   *   --tblr-gutter-y:var(--tblr-page-padding);
   *   min-width:0;
   * }
   * }}}
  */
  def `row-cards`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-1>* {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `row-cols-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-2>* {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `row-cols-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-3>* {
   *   flex:0 0 auto;
   *   width:33.3333333333%;
   * }
   * }}}
  */
  def `row-cols-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-4>* {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `row-cols-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-5>* {
   *   flex:0 0 auto;
   *   width:20%;
   * }
   * }}}
  */
  def `row-cols-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-6>* {
   *   flex:0 0 auto;
   *   width:16.6666666667%;
   * }
   * }}}
  */
  def `row-cols-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-auto>* {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `row-cols-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-1>* {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `row-cols-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-2>* {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `row-cols-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-3>* {
   *   flex:0 0 auto;
   *   width:33.3333333333%;
   * }
   * }}}
  */
  def `row-cols-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-4>* {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `row-cols-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-5>* {
   *   flex:0 0 auto;
   *   width:20%;
   * }
   * }}}
  */
  def `row-cols-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-6>* {
   *   flex:0 0 auto;
   *   width:16.6666666667%;
   * }
   * }}}
  */
  def `row-cols-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-lg-auto>* {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `row-cols-lg-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-1>* {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `row-cols-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-2>* {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `row-cols-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-3>* {
   *   flex:0 0 auto;
   *   width:33.3333333333%;
   * }
   * }}}
  */
  def `row-cols-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-4>* {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `row-cols-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-5>* {
   *   flex:0 0 auto;
   *   width:20%;
   * }
   * }}}
  */
  def `row-cols-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-6>* {
   *   flex:0 0 auto;
   *   width:16.6666666667%;
   * }
   * }}}
  */
  def `row-cols-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-md-auto>* {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `row-cols-md-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-1>* {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `row-cols-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-2>* {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `row-cols-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-3>* {
   *   flex:0 0 auto;
   *   width:33.3333333333%;
   * }
   * }}}
  */
  def `row-cols-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-4>* {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `row-cols-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-5>* {
   *   flex:0 0 auto;
   *   width:20%;
   * }
   * }}}
  */
  def `row-cols-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-6>* {
   *   flex:0 0 auto;
   *   width:16.6666666667%;
   * }
   * }}}
  */
  def `row-cols-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-sm-auto>* {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `row-cols-sm-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-1>* {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `row-cols-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-2>* {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `row-cols-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-3>* {
   *   flex:0 0 auto;
   *   width:33.3333333333%;
   * }
   * }}}
  */
  def `row-cols-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-4>* {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `row-cols-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-5>* {
   *   flex:0 0 auto;
   *   width:20%;
   * }
   * }}}
  */
  def `row-cols-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-6>* {
   *   flex:0 0 auto;
   *   width:16.6666666667%;
   * }
   * }}}
  */
  def `row-cols-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xl-auto>* {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `row-cols-xl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-1>* {
   *   flex:0 0 auto;
   *   width:100%;
   * }
   * }}}
  */
  def `row-cols-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-2>* {
   *   flex:0 0 auto;
   *   width:50%;
   * }
   * }}}
  */
  def `row-cols-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-3>* {
   *   flex:0 0 auto;
   *   width:33.3333333333%;
   * }
   * }}}
  */
  def `row-cols-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-4>* {
   *   flex:0 0 auto;
   *   width:25%;
   * }
   * }}}
  */
  def `row-cols-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-5>* {
   *   flex:0 0 auto;
   *   width:20%;
   * }
   * }}}
  */
  def `row-cols-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-6>* {
   *   flex:0 0 auto;
   *   width:16.6666666667%;
   * }
   * }}}
  */
  def `row-cols-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-cols-xxl-auto>* {
   *   flex:0 0 auto;
   *   width:auto;
   * }
   * }}}
  */
  def `row-cols-xxl-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-deck>.col,
   * .row-deck>[class*=col-] {
   *   display:flex;
   *   align-items:stretch;
   * }
   * }}}
  */
  def `row-deck`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-0 { row-gap:0 !important; }
   * }}}
  */
  def `row-gap-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-1 { row-gap:.25rem !important; }
   * }}}
  */
  def `row-gap-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-2 { row-gap:.5rem !important; }
   * }}}
  */
  def `row-gap-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-3 { row-gap:1rem !important; }
   * }}}
  */
  def `row-gap-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-4 { row-gap:1.5rem !important; }
   * }}}
  */
  def `row-gap-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-5 { row-gap:2rem !important; }
   * }}}
  */
  def `row-gap-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-6 { row-gap:3rem !important; }
   * }}}
  */
  def `row-gap-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-7 { row-gap:5rem !important; }
   * }}}
  */
  def `row-gap-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-8 { row-gap:8rem !important; }
   * }}}
  */
  def `row-gap-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-0 { row-gap:0 !important; }
   * }}}
  */
  def `row-gap-lg-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-1 { row-gap:.25rem !important; }
   * }}}
  */
  def `row-gap-lg-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-2 { row-gap:.5rem !important; }
   * }}}
  */
  def `row-gap-lg-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-3 { row-gap:1rem !important; }
   * }}}
  */
  def `row-gap-lg-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-4 { row-gap:1.5rem !important; }
   * }}}
  */
  def `row-gap-lg-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-5 { row-gap:2rem !important; }
   * }}}
  */
  def `row-gap-lg-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-6 { row-gap:3rem !important; }
   * }}}
  */
  def `row-gap-lg-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-7 { row-gap:5rem !important; }
   * }}}
  */
  def `row-gap-lg-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-lg-8 { row-gap:8rem !important; }
   * }}}
  */
  def `row-gap-lg-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-0 { row-gap:0 !important; }
   * }}}
  */
  def `row-gap-md-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-1 { row-gap:.25rem !important; }
   * }}}
  */
  def `row-gap-md-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-2 { row-gap:.5rem !important; }
   * }}}
  */
  def `row-gap-md-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-3 { row-gap:1rem !important; }
   * }}}
  */
  def `row-gap-md-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-4 { row-gap:1.5rem !important; }
   * }}}
  */
  def `row-gap-md-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-5 { row-gap:2rem !important; }
   * }}}
  */
  def `row-gap-md-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-6 { row-gap:3rem !important; }
   * }}}
  */
  def `row-gap-md-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-7 { row-gap:5rem !important; }
   * }}}
  */
  def `row-gap-md-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-md-8 { row-gap:8rem !important; }
   * }}}
  */
  def `row-gap-md-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-0 { row-gap:0 !important; }
   * }}}
  */
  def `row-gap-sm-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-1 { row-gap:.25rem !important; }
   * }}}
  */
  def `row-gap-sm-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-2 { row-gap:.5rem !important; }
   * }}}
  */
  def `row-gap-sm-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-3 { row-gap:1rem !important; }
   * }}}
  */
  def `row-gap-sm-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-4 { row-gap:1.5rem !important; }
   * }}}
  */
  def `row-gap-sm-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-5 { row-gap:2rem !important; }
   * }}}
  */
  def `row-gap-sm-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-6 { row-gap:3rem !important; }
   * }}}
  */
  def `row-gap-sm-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-7 { row-gap:5rem !important; }
   * }}}
  */
  def `row-gap-sm-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-sm-8 { row-gap:8rem !important; }
   * }}}
  */
  def `row-gap-sm-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-0 { row-gap:0 !important; }
   * }}}
  */
  def `row-gap-xl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-1 { row-gap:.25rem !important; }
   * }}}
  */
  def `row-gap-xl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-2 { row-gap:.5rem !important; }
   * }}}
  */
  def `row-gap-xl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-3 { row-gap:1rem !important; }
   * }}}
  */
  def `row-gap-xl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-4 { row-gap:1.5rem !important; }
   * }}}
  */
  def `row-gap-xl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-5 { row-gap:2rem !important; }
   * }}}
  */
  def `row-gap-xl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-6 { row-gap:3rem !important; }
   * }}}
  */
  def `row-gap-xl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-7 { row-gap:5rem !important; }
   * }}}
  */
  def `row-gap-xl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xl-8 { row-gap:8rem !important; }
   * }}}
  */
  def `row-gap-xl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-0 { row-gap:0 !important; }
   * }}}
  */
  def `row-gap-xxl-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-1 { row-gap:.25rem !important; }
   * }}}
  */
  def `row-gap-xxl-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-2 { row-gap:.5rem !important; }
   * }}}
  */
  def `row-gap-xxl-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-3 { row-gap:1rem !important; }
   * }}}
  */
  def `row-gap-xxl-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-4 { row-gap:1.5rem !important; }
   * }}}
  */
  def `row-gap-xxl-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-5 { row-gap:2rem !important; }
   * }}}
  */
  def `row-gap-xxl-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-6 { row-gap:3rem !important; }
   * }}}
  */
  def `row-gap-xxl-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-7 { row-gap:5rem !important; }
   * }}}
  */
  def `row-gap-xxl-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-gap-xxl-8 { row-gap:8rem !important; }
   * }}}
  */
  def `row-gap-xxl-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-lg {
   *   margin-right:-3rem;
   *   margin-left:-3rem;
   * }
   * }}}
  */
  def `row-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-md {
   *   margin-right:-1.5rem;
   *   margin-left:-1.5rem;
   * }
   * }}}
  */
  def `row-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .row-sm {
   *   margin-right:-.375rem;
   *   margin-left:-.375rem;
   * }
   * }}}
  */
  def `row-sm`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def rtl: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .dl,
   * .highlight .s,
   * .highlight .s2 { color:#b5f4a5; }
   * }}}
  */
  def s: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .mi,
   * .highlight .s1 { color:#d9a9ff; }
   * }}}
  */
  def s1: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .highlight .dl,
   * .highlight .s,
   * .highlight .s2 { color:#b5f4a5; }
   * }}}
  */
  def s2: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .scroll-x,
   * .scroll-y {
   *   overflow:hidden;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `scroll-x`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .scroll-x,
   * .scroll-y {
   *   overflow:hidden;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `scroll-y`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .scrollable {
   *   overflow-x:hidden;
   *   overflow-y:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def scrollable: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-btn {
   *   position:fixed;
   *   right:-1px;
   *   top:10rem;
   *   border-top-right-radius:0;
   *   border-bottom-right-radius:0;
   *   box-shadow:rgba(var(--tblr-body-color-rgb),.04) 0 2px 4px 0;
   * }
   * }}}
  */
  def `settings-btn`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-scheme {
   *   display:inline-block;
   *   border-radius:50%;
   *   height:3rem;
   *   width:3rem;
   *   position:relative;
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color);
   *   box-shadow:rgba(var(--tblr-body-color-rgb),.04) 0 2px 4px 0;
   * }
   * }}}
  */
  def `settings-scheme`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-scheme-colored { background-image:linear-gradient(135deg,var(--tblr-primary) 50%,#fcfdfe 50%); }
   * }}}
  */
  def `settings-scheme-colored`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-scheme-dark { background:#182433; }
   * }}}
  */
  def `settings-scheme-dark`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-scheme-light { background:linear-gradient(135deg,#fff 50%,#fcfdfe 50%); }
   * }}}
  */
  def `settings-scheme-light`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-scheme-mixed { background-image:linear-gradient(135deg,#182433 50%,#fff 50%); }
   * }}}
  */
  def `settings-scheme-mixed`: BtsClass = this

  /**
   * Files:demo.min.css;demo.rtl.min.css
   * {{{
   * .settings-scheme-transparent { background:#fcfdfe; }
   * }}}
  */
  def `settings-scheme-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .shadow { box-shadow:rgba(var(--tblr-body-color-rgb),.04) 0 2px 4px 0 !important; }
   * }}}
  */
  def shadow: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .shadow-lg { box-shadow:0 1rem 3rem rgba(0,0,0,.175) !important; }
   * }}}
  */
  def `shadow-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .shadow-none { box-shadow:none !important; }
   * }}}
  */
  def `shadow-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .shadow-sm { box-shadow:0 .125rem .25rem rgba(0,0,0,.075) !important; }
   * }}}
  */
  def `shadow-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .btn-check:checked+.btn,
   * .btn.active,
   * .btn.show,
   * .btn:first-child:active,
   * :not(.btn-check)+.btn:active {
   *   color:var(--tblr-btn-active-color);
   *   background-color:var(--tblr-btn-active-bg);
   *   border-color:var(--tblr-btn-active-border-color);
   *   box-shadow:var(--tblr-btn-active-shadow);
   * }
   * }}}
  */
  def show: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast.showing { opacity:0; }
   * }}}
  */
  def showing: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.single .ts-control,
   * .ts-wrapper.single .ts-control input { cursor:pointer; }
   * }}}
  */
  def single: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .small,
   * small { font-size:85.714285%; }
   * }}}
  */
  def small: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x {
   *   display:flex;
   *   gap:1rem;
   * }
   * }}}
  */
  def `space-x`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-0 {
   *   display:flex;
   *   gap:0;
   * }
   * }}}
  */
  def `space-x-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-1 {
   *   display:flex;
   *   gap:.25rem;
   * }
   * }}}
  */
  def `space-x-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-2 {
   *   display:flex;
   *   gap:.5rem;
   * }
   * }}}
  */
  def `space-x-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-3 {
   *   display:flex;
   *   gap:1rem;
   * }
   * }}}
  */
  def `space-x-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-4 {
   *   display:flex;
   *   gap:1.5rem;
   * }
   * }}}
  */
  def `space-x-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-5 {
   *   display:flex;
   *   gap:2rem;
   * }
   * }}}
  */
  def `space-x-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-6 {
   *   display:flex;
   *   gap:3rem;
   * }
   * }}}
  */
  def `space-x-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-7 {
   *   display:flex;
   *   gap:5rem;
   * }
   * }}}
  */
  def `space-x-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-x-8 {
   *   display:flex;
   *   gap:8rem;
   * }
   * }}}
  */
  def `space-x-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y {
   *   display:flex;
   *   flex-direction:column;
   *   gap:1rem;
   * }
   * }}}
  */
  def `space-y`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-0 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:0;
   * }
   * }}}
  */
  def `space-y-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-1 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:.25rem;
   * }
   * }}}
  */
  def `space-y-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-2 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:.5rem;
   * }
   * }}}
  */
  def `space-y-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-3 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:1rem;
   * }
   * }}}
  */
  def `space-y-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-4 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:1.5rem;
   * }
   * }}}
  */
  def `space-y-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-5 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:2rem;
   * }
   * }}}
  */
  def `space-y-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-6 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:3rem;
   * }
   * }}}
  */
  def `space-y-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-7 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:5rem;
   * }
   * }}}
  */
  def `space-y-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .space-y-8 {
   *   display:flex;
   *   flex-direction:column;
   *   gap:8rem;
   * }
   * }}}
  */
  def `space-y-8`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown .spinner {
   *   display:inline-block;
   *   width:30px;
   *   height:30px;
   *   margin:3px .75rem;
   * }
   * }}}
  */
  def spinner: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .spinner-border,
   * .spinner-grow {
   *   display:inline-block;
   *   width:var(--tblr-spinner-width);
   *   height:var(--tblr-spinner-height);
   *   vertical-align:var(--tblr-spinner-vertical-align);
   *   border-radius:50%;
   *   animation:var(--tblr-spinner-animation-speed) linear infinite var(--tblr-spinner-animation-name);
   * }
   * }}}
  */
  def `spinner-border`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .spinner-border-sm {
   *   --tblr-spinner-width:1rem;
   *   --tblr-spinner-height:1rem;
   *   --tblr-spinner-border-width:1px;
   * }
   * }}}
  */
  def `spinner-border-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .spinner-border,
   * .spinner-grow {
   *   display:inline-block;
   *   width:var(--tblr-spinner-width);
   *   height:var(--tblr-spinner-height);
   *   vertical-align:var(--tblr-spinner-vertical-align);
   *   border-radius:50%;
   *   animation:var(--tblr-spinner-animation-speed) linear infinite var(--tblr-spinner-animation-name);
   * }
   * }}}
  */
  def `spinner-grow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .spinner-grow-sm {
   *   --tblr-spinner-width:1rem;
   *   --tblr-spinner-height:1rem;
   * }
   * }}}
  */
  def `spinner-grow-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .stars .star:not(:first-child) { margin-left:.25rem; }
   * }}}
  */
  def star: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .stars {
   *   display:inline-flex;
   *   color:#bbc3cd;
   *   font-size:.75rem;
   * }
   * }}}
  */
  def stars: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .start-0 { left:0 !important; }
   * }}}
  */
  def `start-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .start-100 { left:100% !important; }
   * }}}
  */
  def `start-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .start-50 { left:50% !important; }
   * }}}
  */
  def `start-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status {
   *   --tblr-status-height:1.5rem;
   *   --tblr-status-color:#667382;
   *   --tblr-status-color-rgb:102,115,130;
   *   display:inline-flex;
   *   align-items:center;
   *   height:var(--tblr-status-height);
   *   padding:.25rem .75rem;
   *   gap:.5rem;
   *   color:var(--tblr-status-color);
   *   background:rgba(var(--tblr-status-color-rgb),.1);
   *   font-size:.875rem;
   *   text-transform:none;
   *   letter-spacing:normal;
   *   border-radius:100rem;
   *   font-weight:var(--tblr-font-weight-medium);
   *   line-height:1;
   *   margin:0;
   * }
   * }}}
  */
  def status: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-azure {
   *   --tblr-status-color:#4299e1;
   *   --tblr-status-color-rgb:66,153,225;
   * }
   * }}}
  */
  def `status-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-bitbucket {
   *   --tblr-status-color:#0052cc;
   *   --tblr-status-color-rgb:0,82,204;
   * }
   * }}}
  */
  def `status-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-blue {
   *   --tblr-status-color:#0054a6;
   *   --tblr-status-color-rgb:0,84,166;
   * }
   * }}}
  */
  def `status-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-cyan {
   *   --tblr-status-color:#17a2b8;
   *   --tblr-status-color-rgb:23,162,184;
   * }
   * }}}
  */
  def `status-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-danger {
   *   --tblr-status-color:#d63939;
   *   --tblr-status-color-rgb:214,57,57;
   * }
   * }}}
  */
  def `status-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-dark {
   *   --tblr-status-color:#182433;
   *   --tblr-status-color-rgb:24,36,51;
   * }
   * }}}
  */
  def `status-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status .status-dot { background:var(--tblr-status-color); }
   * }}}
  */
  def `status-dot`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-dot-animated:before {
   *   content:"";
   *   position:absolute;
   *   inset:0;
   *   z-index:0;
   *   background:inherit;
   *   border-radius:inherit;
   *   opacity:.6;
   *   animation:1s linear 2s backwards infinite status-pulsate-tertiary;
   * }
   * }}}
  */
  def `status-dot-animated`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-dribbble {
   *   --tblr-status-color:#ea4c89;
   *   --tblr-status-color-rgb:234,76,137;
   * }
   * }}}
  */
  def `status-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-facebook {
   *   --tblr-status-color:#1877f2;
   *   --tblr-status-color-rgb:24,119,242;
   * }
   * }}}
  */
  def `status-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-flickr {
   *   --tblr-status-color:#0063dc;
   *   --tblr-status-color-rgb:0,99,220;
   * }
   * }}}
  */
  def `status-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-github {
   *   --tblr-status-color:#181717;
   *   --tblr-status-color-rgb:24,23,23;
   * }
   * }}}
  */
  def `status-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-google {
   *   --tblr-status-color:#dc4e41;
   *   --tblr-status-color-rgb:220,78,65;
   * }
   * }}}
  */
  def `status-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-green {
   *   --tblr-status-color:#2fb344;
   *   --tblr-status-color-rgb:47,179,68;
   * }
   * }}}
  */
  def `status-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-indicator {
   *   --tblr-status-indicator-size:2.5rem;
   *   --tblr-status-indicator-color:var(--tblr-status-color,#667382);
   *   display:block;
   *   position:relative;
   *   width:var(--tblr-status-indicator-size);
   *   height:var(--tblr-status-indicator-size);
   * }
   * }}}
  */
  def `status-indicator`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-indicator-animated .status-indicator-circle:nth-child(1) { animation:2s linear 1s infinite backwards status-pulsate-main; }
   * }}}
  */
  def `status-indicator-animated`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-indicator-circle {
   *   --tblr-status-circle-size:.75rem;
   *   position:absolute;
   *   left:50%;
   *   top:50%;
   *   margin:calc(var(--tblr-status-circle-size)/-2) 0 0 calc(var(--tblr-status-circle-size)/-2);
   *   width:var(--tblr-status-circle-size);
   *   height:var(--tblr-status-circle-size);
   *   border-radius:100rem;
   *   background:var(--tblr-status-color);
   * }
   * }}}
  */
  def `status-indicator-circle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-indigo {
   *   --tblr-status-color:#4263eb;
   *   --tblr-status-color-rgb:66,99,235;
   * }
   * }}}
  */
  def `status-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-info {
   *   --tblr-status-color:#4299e1;
   *   --tblr-status-color-rgb:66,153,225;
   * }
   * }}}
  */
  def `status-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-instagram {
   *   --tblr-status-color:#e4405f;
   *   --tblr-status-color-rgb:228,64,95;
   * }
   * }}}
  */
  def `status-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-light {
   *   --tblr-status-color:#fcfdfe;
   *   --tblr-status-color-rgb:252,253,254;
   * }
   * }}}
  */
  def `status-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-lime {
   *   --tblr-status-color:#74b816;
   *   --tblr-status-color-rgb:116,184,22;
   * }
   * }}}
  */
  def `status-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-linkedin {
   *   --tblr-status-color:#0a66c2;
   *   --tblr-status-color-rgb:10,102,194;
   * }
   * }}}
  */
  def `status-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-lite {
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color) !important;
   *   background:0 0 !important;
   *   color:var(--tblr-body-color) !important;
   * }
   * }}}
  */
  def `status-lite`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-muted {
   *   --tblr-status-color:#667382;
   *   --tblr-status-color-rgb:102,115,130;
   * }
   * }}}
  */
  def `status-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-orange {
   *   --tblr-status-color:#f76707;
   *   --tblr-status-color-rgb:247,103,7;
   * }
   * }}}
  */
  def `status-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-pink {
   *   --tblr-status-color:#d6336c;
   *   --tblr-status-color-rgb:214,51,108;
   * }
   * }}}
  */
  def `status-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-pinterest {
   *   --tblr-status-color:#bd081c;
   *   --tblr-status-color-rgb:189,8,28;
   * }
   * }}}
  */
  def `status-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-primary {
   *   --tblr-status-color:#0054a6;
   *   --tblr-status-color-rgb:0,84,166;
   * }
   * }}}
  */
  def `status-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-purple {
   *   --tblr-status-color:#ae3ec9;
   *   --tblr-status-color-rgb:174,62,201;
   * }
   * }}}
  */
  def `status-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-red {
   *   --tblr-status-color:#d63939;
   *   --tblr-status-color-rgb:214,57,57;
   * }
   * }}}
  */
  def `status-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-rss {
   *   --tblr-status-color:#ffa500;
   *   --tblr-status-color-rgb:255,165,0;
   * }
   * }}}
  */
  def `status-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-secondary {
   *   --tblr-status-color:#667382;
   *   --tblr-status-color-rgb:102,115,130;
   * }
   * }}}
  */
  def `status-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-success {
   *   --tblr-status-color:#2fb344;
   *   --tblr-status-color-rgb:47,179,68;
   * }
   * }}}
  */
  def `status-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-tabler {
   *   --tblr-status-color:#0054a6;
   *   --tblr-status-color-rgb:0,84,166;
   * }
   * }}}
  */
  def `status-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-teal {
   *   --tblr-status-color:#0ca678;
   *   --tblr-status-color-rgb:12,166,120;
   * }
   * }}}
  */
  def `status-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-twitter {
   *   --tblr-status-color:#1da1f2;
   *   --tblr-status-color-rgb:29,161,242;
   * }
   * }}}
  */
  def `status-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-vimeo {
   *   --tblr-status-color:#1ab7ea;
   *   --tblr-status-color-rgb:26,183,234;
   * }
   * }}}
  */
  def `status-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-vk {
   *   --tblr-status-color:#6383a8;
   *   --tblr-status-color-rgb:99,131,168;
   * }
   * }}}
  */
  def `status-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-warning {
   *   --tblr-status-color:#f76707;
   *   --tblr-status-color-rgb:247,103,7;
   * }
   * }}}
  */
  def `status-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-yellow {
   *   --tblr-status-color:#f59f00;
   *   --tblr-status-color-rgb:245,159,0;
   * }
   * }}}
  */
  def `status-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .status-youtube {
   *   --tblr-status-color:#ff0000;
   *   --tblr-status-color-rgb:255,0,0;
   * }
   * }}}
  */
  def `status-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .step-item {
   *   position:relative;
   *   flex:1 1 0;
   *   min-height:1rem;
   *   margin-top:0;
   *   color:inherit;
   *   text-align:center;
   *   cursor:default;
   *   padding-top:calc(var(--tblr-steps-dot-size));
   * }
   * }}}
  */
  def `step-item`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps {
   *   --tblr-steps-color:var(--tblr-primary);
   *   --tblr-steps-inactive-color:var(--tblr-border-color);
   *   --tblr-steps-dot-size:.5rem;
   *   --tblr-steps-border-width:2px;
   *   display:flex;
   *   flex-wrap:nowrap;
   *   width:100%;
   *   padding:0;
   *   margin:0;
   *   list-style:none;
   * }
   * }}}
  */
  def steps: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-azure { --tblr-steps-color:var(--tblr-azure); }
   * }}}
  */
  def `steps-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-blue { --tblr-steps-color:var(--tblr-blue); }
   * }}}
  */
  def `steps-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-counter {
   *   --tblr-steps-dot-size:1.5rem;
   *   counter-reset:steps;
   * }
   * }}}
  */
  def `steps-counter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-cyan { --tblr-steps-color:var(--tblr-cyan); }
   * }}}
  */
  def `steps-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-green { --tblr-steps-color:var(--tblr-green); }
   * }}}
  */
  def `steps-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-indigo { --tblr-steps-color:var(--tblr-indigo); }
   * }}}
  */
  def `steps-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-lime { --tblr-steps-color:var(--tblr-lime); }
   * }}}
  */
  def `steps-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-orange { --tblr-steps-color:var(--tblr-orange); }
   * }}}
  */
  def `steps-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-pink { --tblr-steps-color:var(--tblr-pink); }
   * }}}
  */
  def `steps-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-purple { --tblr-steps-color:var(--tblr-purple); }
   * }}}
  */
  def `steps-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-red { --tblr-steps-color:var(--tblr-red); }
   * }}}
  */
  def `steps-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-teal { --tblr-steps-color:var(--tblr-teal); }
   * }}}
  */
  def `steps-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-vertical {
   *   --tblr-steps-dot-offset:6px;
   *   flex-direction:column;
   * }
   * }}}
  */
  def `steps-vertical`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .steps-yellow { --tblr-steps-color:var(--tblr-yellow); }
   * }}}
  */
  def `steps-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-bottom {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   bottom:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-lg-bottom {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   bottom:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-lg-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-lg-top {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-lg-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-md-bottom {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   bottom:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-md-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-md-top {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-md-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-sm-bottom {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   bottom:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-sm-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-sm-top {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-sm-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-top {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-xl-bottom {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   bottom:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-xl-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-xl-top {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-xl-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-xxl-bottom {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   bottom:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-xxl-bottom`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .sticky-xxl-top {
   *   position:-webkit-sticky;
   *   position:sticky;
   *   top:0;
   *   z-index:1020;
   * }
   * }}}
  */
  def `sticky-xxl-top`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .stretched-link::after {
   *   position:absolute;
   *   top:0;
   *   right:0;
   *   bottom:0;
   *   left:0;
   *   z-index:1;
   *   content:"";
   * }
   * }}}
  */
  def `stretched-link`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .strong,
   * b,
   * strong { font-weight:var(--tblr-font-weight-bold); }
   * }}}
  */
  def strong: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .subheader {
   *   font-size:.625rem;
   *   font-weight:var(--tblr-font-weight-bold);
   *   text-transform:uppercase;
   *   letter-spacing:.04em;
   *   line-height:1rem;
   *   color:var(--tblr-secondary);
   * }
   * }}}
  */
  def subheader: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .subpixel-antialiased {
   *   -webkit-font-smoothing:auto;
   *   -moz-osx-font-smoothing:auto;
   * }
   * }}}
  */
  def `subpixel-antialiased`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon {
   *   display:inline-block;
   *   line-height:1;
   *   border:0;
   *   padding:0;
   *   background:0 0;
   *   width:1.25rem;
   *   height:1.25rem;
   *   vertical-align:bottom;
   *   position:relative;
   *   cursor:pointer;
   * }
   * }}}
  */
  def `switch-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon .switch-icon-a,
   * .switch-icon .switch-icon-b {
   *   display:block;
   *   width:100%;
   *   height:100%;
   * }
   * }}}
  */
  def `switch-icon-a`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon .switch-icon-a,
   * .switch-icon .switch-icon-b {
   *   display:block;
   *   width:100%;
   *   height:100%;
   * }
   * }}}
  */
  def `switch-icon-b`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-fade .switch-icon-a,
   * .switch-icon-fade .switch-icon-b { transition:opacity .5s; }
   * }}}
  */
  def `switch-icon-fade`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-flip { perspective:10em; }
   * }}}
  */
  def `switch-icon-flip`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-scale .switch-icon-a,
   * .switch-icon-scale .switch-icon-b { transition:opacity .5s,transform 0s .5s; }
   * }}}
  */
  def `switch-icon-scale`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-slide-down,
   * .switch-icon-slide-left,
   * .switch-icon-slide-right,
   * .switch-icon-slide-up { overflow:hidden; }
   * }}}
  */
  def `switch-icon-slide-down`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-slide-down,
   * .switch-icon-slide-left,
   * .switch-icon-slide-right,
   * .switch-icon-slide-up { overflow:hidden; }
   * }}}
  */
  def `switch-icon-slide-left`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-slide-down,
   * .switch-icon-slide-left,
   * .switch-icon-slide-right,
   * .switch-icon-slide-up { overflow:hidden; }
   * }}}
  */
  def `switch-icon-slide-right`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .switch-icon-slide-down,
   * .switch-icon-slide-left,
   * .switch-icon-slide-right,
   * .switch-icon-slide-up { overflow:hidden; }
   * }}}
  */
  def `switch-icon-slide-up`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tab-content>.tab-pane { display:none; }
   * }}}
  */
  def `tab-content`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tab-content>.tab-pane { display:none; }
   * }}}
  */
  def `tab-pane`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .markdown>table,
   * .table {
   *   --tblr-table-color-type:initial;
   *   --tblr-table-bg-type:initial;
   *   --tblr-table-color-state:initial;
   *   --tblr-table-bg-state:initial;
   *   --tblr-table-color:inherit;
   *   --tblr-table-bg:transparent;
   *   --tblr-table-border-color:var(--tblr-border-color-translucent);
   *   --tblr-table-accent-bg:transparent;
   *   --tblr-table-striped-color:inherit;
   *   --tblr-table-striped-bg:var(--tblr-bg-surface-tertiary);
   *   --tblr-table-active-color:inherit;
   *   --tblr-table-active-bg:rgba(0,0,0,0.1);
   *   --tblr-table-hover-color:inherit;
   *   --tblr-table-hover-bg:rgba(0,0,0,0.075);
   *   width:100%;
   *   margin-bottom:1rem;
   *   vertical-align:top;
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def table: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-active {
   *   --tblr-table-color-state:var(--tblr-table-active-color);
   *   --tblr-table-bg-state:var(--tblr-table-active-bg);
   * }
   * }}}
  */
  def `table-active`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .markdown>table>:not(caption)>*,
   * .table-bordered>:not(caption)>* { border-width:var(--tblr-border-width) 0; }
   * }}}
  */
  def `table-bordered`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-borderless>:not(caption)>*>* { border-bottom-width:0; }
   * }}}
  */
  def `table-borderless`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-center>:not(caption)>*>* { text-align:center; }
   * }}}
  */
  def `table-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-danger {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#f7d7d7;
   *   --tblr-table-border-color:#e1c5c7;
   *   --tblr-table-striped-bg:#eccecf;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#e1c5c7;
   *   --tblr-table-active-color:#fcfdfe;
   *   --tblr-table-hover-bg:#e6cacb;
   *   --tblr-table-hover-color:#fcfdfe;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-dark {
   *   --tblr-table-color:#fcfdfe;
   *   --tblr-table-bg:#182433;
   *   --tblr-table-border-color:#2f3a47;
   *   --tblr-table-striped-bg:#232f3d;
   *   --tblr-table-striped-color:#fcfdfe;
   *   --tblr-table-active-bg:#2f3a47;
   *   --tblr-table-active-color:#fcfdfe;
   *   --tblr-table-hover-bg:#293442;
   *   --tblr-table-hover-color:#fcfdfe;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-group-divider { border-top:calc(var(--tblr-border-width)*2) solid var(--tblr-border-color-translucent); }
   * }}}
  */
  def `table-group-divider`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-hover>tbody>tr:hover>* {
   *   --tblr-table-color-state:var(--tblr-table-hover-color);
   *   --tblr-table-bg-state:var(--tblr-table-hover-bg);
   * }
   * }}}
  */
  def `table-hover`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-info {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#d9ebf9;
   *   --tblr-table-border-color:#c6d7e5;
   *   --tblr-table-striped-bg:#cfe1ef;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#c6d7e5;
   *   --tblr-table-active-color:#182433;
   *   --tblr-table-hover-bg:#cbdcea;
   *   --tblr-table-hover-color:#182433;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-light {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#fcfdfe;
   *   --tblr-table-border-color:#e5e7ea;
   *   --tblr-table-striped-bg:#f1f2f4;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#e5e7ea;
   *   --tblr-table-active-color:#182433;
   *   --tblr-table-hover-bg:#ebedef;
   *   --tblr-table-hover-color:#182433;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-mobile { display:block; }
   * }}}
  */
  def `table-mobile`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-mobile-lg .btn { display:block; }
   * }}}
  */
  def `table-mobile-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-mobile-md .btn { display:block; }
   * }}}
  */
  def `table-mobile-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-mobile-sm .btn { display:block; }
   * }}}
  */
  def `table-mobile-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-mobile-xl .btn { display:block; }
   * }}}
  */
  def `table-mobile-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-mobile-xxl .btn { display:block; }
   * }}}
  */
  def `table-mobile-xxl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-nowrap>:not(caption)>*>* { white-space:nowrap; }
   * }}}
  */
  def `table-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-primary {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#ccdded;
   *   --tblr-table-border-color:#bacbda;
   *   --tblr-table-striped-bg:#c3d4e4;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#bacbda;
   *   --tblr-table-active-color:#fcfdfe;
   *   --tblr-table-hover-bg:#bfcfdf;
   *   --tblr-table-hover-color:#fcfdfe;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-responsive {
   *   overflow-x:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `table-responsive`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-responsive-lg {
   *   overflow-x:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `table-responsive-lg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-responsive-md {
   *   overflow-x:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `table-responsive-md`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-responsive-sm {
   *   overflow-x:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `table-responsive-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-responsive-xl {
   *   overflow-x:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `table-responsive-xl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-responsive-xxl {
   *   overflow-x:auto;
   *   -webkit-overflow-scrolling:touch;
   * }
   * }}}
  */
  def `table-responsive-xxl`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-secondary {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#e0e3e6;
   *   --tblr-table-border-color:#ccd0d4;
   *   --tblr-table-striped-bg:#d6d9dd;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#ccd0d4;
   *   --tblr-table-active-color:#fcfdfe;
   *   --tblr-table-hover-bg:#d1d5d9;
   *   --tblr-table-hover-color:#182433;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-sm>:not(caption)>*>* { padding:.25rem .25rem; }
   * }}}
  */
  def `table-sm`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-sort {
   *   font:inherit;
   *   color:inherit;
   *   text-transform:inherit;
   *   letter-spacing:inherit;
   *   border:0;
   *   background:inherit;
   *   display:block;
   *   width:100%;
   *   text-align:inherit;
   *   transition:color .3s;
   *   margin:-.5rem -.75rem;
   *   padding:.5rem .75rem;
   * }
   * }}}
  */
  def `table-sort`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-striped>tbody>tr:nth-of-type(even)>* {
   *   --tblr-table-color-type:var(--tblr-table-striped-color);
   *   --tblr-table-bg-type:var(--tblr-table-striped-bg);
   * }
   * }}}
  */
  def `table-striped`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-striped-columns>:not(caption)>tr>:nth-child(even) {
   *   --tblr-table-color-type:var(--tblr-table-striped-color);
   *   --tblr-table-bg-type:var(--tblr-table-striped-bg);
   * }
   * }}}
  */
  def `table-striped-columns`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-success {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#d5f0da;
   *   --tblr-table-border-color:#c2dcc9;
   *   --tblr-table-striped-bg:#cce6d2;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#c2dcc9;
   *   --tblr-table-active-color:#182433;
   *   --tblr-table-hover-bg:#c7e1cd;
   *   --tblr-table-hover-color:#182433;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-transparent thead th { background:0 0; }
   * }}}
  */
  def `table-transparent`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-vcenter>:not(caption)>*>* { vertical-align:middle; }
   * }}}
  */
  def `table-vcenter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .table-warning {
   *   --tblr-table-color:#182433;
   *   --tblr-table-bg:#fde1cd;
   *   --tblr-table-border-color:#e6cebe;
   *   --tblr-table-striped-bg:#f2d8c5;
   *   --tblr-table-striped-color:#182433;
   *   --tblr-table-active-bg:#e6cebe;
   *   --tblr-table-active-color:#182433;
   *   --tblr-table-hover-bg:#ecd3c1;
   *   --tblr-table-hover-color:#182433;
   *   color:var(--tblr-table-color);
   *   border-color:var(--tblr-table-border-color);
   * }
   * }}}
  */
  def `table-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag {
   *   --tblr-tag-height:1.5rem;
   *   border:1px solid var(--tblr-border-color);
   *   display:inline-flex;
   *   align-items:center;
   *   height:var(--tblr-tag-height);
   *   border-radius:var(--tblr-border-radius);
   *   padding:0 .5rem;
   *   background:var(--tblr-bg-surface);
   *   box-shadow:var(--tblr-box-shadow-input);
   *   gap:.5rem;
   * }
   * }}}
  */
  def tag: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag-avatar,
   * .tag-check,
   * .tag-flag,
   * .tag-icon,
   * .tag-payment { margin-left:-.25rem; }
   * }}}
  */
  def `tag-avatar`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag-badge {
   *   --tblr-badge-font-size:0.625rem;
   *   --tblr-badge-padding-x:.25rem;
   *   --tblr-badge-padding-y:.125rem;
   *   margin-right:-.25rem;
   * }
   * }}}
  */
  def `tag-badge`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag-avatar,
   * .tag-check,
   * .tag-flag,
   * .tag-icon,
   * .tag-payment { margin-left:-.25rem; }
   * }}}
  */
  def `tag-check`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag-avatar,
   * .tag-check,
   * .tag-flag,
   * .tag-icon,
   * .tag-payment { margin-left:-.25rem; }
   * }}}
  */
  def `tag-flag`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag-avatar,
   * .tag-check,
   * .tag-flag,
   * .tag-icon,
   * .tag-payment { margin-left:-.25rem; }
   * }}}
  */
  def `tag-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tag-avatar,
   * .tag-check,
   * .tag-flag,
   * .tag-icon,
   * .tag-payment { margin-left:-.25rem; }
   * }}}
  */
  def `tag-payment`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tags-list {
   *   --tblr-list-gap:0.5rem;
   *   display:flex;
   *   flex-wrap:wrap;
   *   gap:var(--tblr-list-gap);
   * }
   * }}}
  */
  def `tags-list`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .td-truncate {
   *   max-width:1px;
   *   width:100%;
   * }
   * }}}
  */
  def `td-truncate`: BtsClass = this

  /**
   * Files:App.css
   * {{{
   * .test-demo { background-color:aliceblue; }
   * }}}
  */
  def `test-demo`: BtsClass = this

  /**
   * Files:App.css
   * {{{
   * .test2 { text-align:center; }
   * }}}
  */
  def test2: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-azure {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-azure-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-azure-fg { color:var(--tblr-azure-fg) !important; }
   * }}}
  */
  def `text-azure-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-azure {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-azure-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-bitbucket {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-bitbucket-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-blue {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-blue-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-cyan {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-cyan-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-danger {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-danger-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-dark {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-dark-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-dribbble {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-dribbble-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-facebook {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-facebook-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-flickr {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-flickr-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-github {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-github-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-google {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-google-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-green {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-green-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-indigo {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-indigo-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-info {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-info-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-instagram {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-instagram-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-light {
   *   color:#182433 !important;
   *   background-color:RGBA(var(--tblr-light-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-lime {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-lime-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-linkedin {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-linkedin-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-muted {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-muted-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-orange {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-orange-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-pink {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-pink-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-pinterest {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-pinterest-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-primary {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-primary-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-purple {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-purple-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-red {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-red-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-rss {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-rss-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-secondary {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-secondary-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-success {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-success-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-tabler {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-tabler-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-teal {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-teal-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-twitter {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-twitter-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-vimeo {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-vimeo-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-vk {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-vk-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-warning {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-warning-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-yellow {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-yellow-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bg-youtube {
   *   color:#fcfdfe !important;
   *   background-color:RGBA(var(--tblr-youtube-rgb),var(--tblr-bg-opacity,1)) !important;
   * }
   * }}}
  */
  def `text-bg-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bitbucket {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-bitbucket-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-bitbucket-fg { color:var(--tblr-bitbucket-fg) !important; }
   * }}}
  */
  def `text-bitbucket-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-black {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-black-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-black`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-black-50 {
   *   --tblr-text-opacity:1;
   *   color:rgba(0,0,0,.5) !important;
   * }
   * }}}
  */
  def `text-black-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-blue {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-blue-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-blue-fg { color:var(--tblr-blue-fg) !important; }
   * }}}
  */
  def `text-blue-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-body {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-body-color-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-body-emphasis {
   *   --tblr-text-opacity:1;
   *   color:var(--tblr-emphasis-color) !important;
   * }
   * }}}
  */
  def `text-body-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-body-secondary {
   *   --tblr-text-opacity:1;
   *   color:var(--tblr-secondary-color) !important;
   * }
   * }}}
  */
  def `text-body-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-body-tertiary {
   *   --tblr-text-opacity:1;
   *   color:var(--tblr-tertiary-color) !important;
   * }
   * }}}
  */
  def `text-body-tertiary`: BtsClass = this

  /**
   * Files:tabler.min.css
   * {{{
   * .text-break {
   *   word-wrap:break-word !important;
   *   word-break:break-word !important;
   * }
   * }}}
  */
  def `text-break`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-capitalize { text-transform:capitalize !important; }
   * }}}
  */
  def `text-capitalize`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-center { text-align:center !important; }
   * }}}
  */
  def `text-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-cyan {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-cyan-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-cyan-fg { color:var(--tblr-cyan-fg) !important; }
   * }}}
  */
  def `text-cyan-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-danger {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-danger-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-danger-emphasis { color:var(--tblr-danger-text-emphasis) !important; }
   * }}}
  */
  def `text-danger-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-danger-fg { color:var(--tblr-danger-fg) !important; }
   * }}}
  */
  def `text-danger-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-dark {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-dark-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-dark-emphasis { color:var(--tblr-dark-text-emphasis) !important; }
   * }}}
  */
  def `text-dark-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-dark-fg { color:var(--tblr-dark-fg) !important; }
   * }}}
  */
  def `text-dark-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-decoration-line-through { text-decoration:line-through !important; }
   * }}}
  */
  def `text-decoration-line-through`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-decoration-none { text-decoration:none !important; }
   * }}}
  */
  def `text-decoration-none`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-decoration-underline { text-decoration:underline !important; }
   * }}}
  */
  def `text-decoration-underline`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-dribbble {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-dribbble-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-dribbble-fg { color:var(--tblr-dribbble-fg) !important; }
   * }}}
  */
  def `text-dribbble-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-end { text-align:right !important; }
   * }}}
  */
  def `text-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-facebook {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-facebook-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-facebook-fg { color:var(--tblr-facebook-fg) !important; }
   * }}}
  */
  def `text-facebook-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-flickr {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-flickr-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-flickr-fg { color:var(--tblr-flickr-fg) !important; }
   * }}}
  */
  def `text-flickr-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-github {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-github-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-github-fg { color:var(--tblr-github-fg) !important; }
   * }}}
  */
  def `text-github-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-google {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-google-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-google-fg { color:var(--tblr-google-fg) !important; }
   * }}}
  */
  def `text-google-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-100-fg { color:#182433 !important; }
   * }}}
  */
  def `text-gray-100-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-200-fg { color:#182433 !important; }
   * }}}
  */
  def `text-gray-200-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-300-fg { color:#182433 !important; }
   * }}}
  */
  def `text-gray-300-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-400-fg { color:#fcfdfe !important; }
   * }}}
  */
  def `text-gray-400-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-50-fg { color:#182433 !important; }
   * }}}
  */
  def `text-gray-50-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-500-fg { color:#fcfdfe !important; }
   * }}}
  */
  def `text-gray-500-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-600-fg { color:#fcfdfe !important; }
   * }}}
  */
  def `text-gray-600-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-700-fg { color:#fcfdfe !important; }
   * }}}
  */
  def `text-gray-700-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-800-fg { color:#fcfdfe !important; }
   * }}}
  */
  def `text-gray-800-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-gray-900-fg { color:#fcfdfe !important; }
   * }}}
  */
  def `text-gray-900-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-green {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-green-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-green-fg { color:var(--tblr-green-fg) !important; }
   * }}}
  */
  def `text-green-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-indigo {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-indigo-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-indigo-fg { color:var(--tblr-indigo-fg) !important; }
   * }}}
  */
  def `text-indigo-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-info {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-info-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-info-emphasis { color:var(--tblr-info-text-emphasis) !important; }
   * }}}
  */
  def `text-info-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-info-fg { color:var(--tblr-info-fg) !important; }
   * }}}
  */
  def `text-info-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-instagram {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-instagram-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-instagram-fg { color:var(--tblr-instagram-fg) !important; }
   * }}}
  */
  def `text-instagram-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-lg-center { text-align:center !important; }
   * }}}
  */
  def `text-lg-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-lg-end { text-align:right !important; }
   * }}}
  */
  def `text-lg-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-lg-start { text-align:left !important; }
   * }}}
  */
  def `text-lg-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-light {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-light-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-light-emphasis { color:var(--tblr-light-text-emphasis) !important; }
   * }}}
  */
  def `text-light-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-light-fg { color:var(--tblr-light-fg) !important; }
   * }}}
  */
  def `text-light-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-lime {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-lime-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-lime-fg { color:var(--tblr-lime-fg) !important; }
   * }}}
  */
  def `text-lime-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-linkedin {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-linkedin-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-linkedin-fg { color:var(--tblr-linkedin-fg) !important; }
   * }}}
  */
  def `text-linkedin-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-lowercase { text-transform:lowercase !important; }
   * }}}
  */
  def `text-lowercase`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-md-center { text-align:center !important; }
   * }}}
  */
  def `text-md-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-md-end { text-align:right !important; }
   * }}}
  */
  def `text-md-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-md-start { text-align:left !important; }
   * }}}
  */
  def `text-md-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-muted {
   *   --tblr-text-opacity:1;
   *   color:var(--tblr-secondary-color) !important;
   * }
   * }}}
  */
  def `text-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-muted-fg { color:var(--tblr-muted-fg) !important; }
   * }}}
  */
  def `text-muted-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-nowrap { white-space:nowrap !important; }
   * }}}
  */
  def `text-nowrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-opacity-100 { --tblr-text-opacity:1; }
   * }}}
  */
  def `text-opacity-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-opacity-25 { --tblr-text-opacity:0.25; }
   * }}}
  */
  def `text-opacity-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-opacity-50 { --tblr-text-opacity:0.5; }
   * }}}
  */
  def `text-opacity-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-opacity-75 { --tblr-text-opacity:0.75; }
   * }}}
  */
  def `text-opacity-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-orange {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-orange-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-orange-fg { color:var(--tblr-orange-fg) !important; }
   * }}}
  */
  def `text-orange-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-pink {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-pink-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-pink-fg { color:var(--tblr-pink-fg) !important; }
   * }}}
  */
  def `text-pink-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-pinterest {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-pinterest-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-pinterest-fg { color:var(--tblr-pinterest-fg) !important; }
   * }}}
  */
  def `text-pinterest-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-primary {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-primary-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-primary-emphasis { color:var(--tblr-primary-text-emphasis) !important; }
   * }}}
  */
  def `text-primary-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-primary-fg { color:var(--tblr-primary-fg) !important; }
   * }}}
  */
  def `text-primary-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-purple {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-purple-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-purple-fg { color:var(--tblr-purple-fg) !important; }
   * }}}
  */
  def `text-purple-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-red {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-red-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-red-fg { color:var(--tblr-red-fg) !important; }
   * }}}
  */
  def `text-red-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-reset {
   *   --tblr-text-opacity:1;
   *   color:inherit !important;
   * }
   * }}}
  */
  def `text-reset`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-rss {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-rss-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-rss-fg { color:var(--tblr-rss-fg) !important; }
   * }}}
  */
  def `text-rss-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-secondary {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-secondary-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-secondary-emphasis { color:var(--tblr-secondary-text-emphasis) !important; }
   * }}}
  */
  def `text-secondary-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-secondary-fg { color:var(--tblr-secondary-fg) !important; }
   * }}}
  */
  def `text-secondary-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-sm-center { text-align:center !important; }
   * }}}
  */
  def `text-sm-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-sm-end { text-align:right !important; }
   * }}}
  */
  def `text-sm-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-sm-start { text-align:left !important; }
   * }}}
  */
  def `text-sm-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-start { text-align:left !important; }
   * }}}
  */
  def `text-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-success {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-success-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-success-emphasis { color:var(--tblr-success-text-emphasis) !important; }
   * }}}
  */
  def `text-success-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-success-fg { color:var(--tblr-success-fg) !important; }
   * }}}
  */
  def `text-success-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-tabler {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-tabler-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-tabler-fg { color:var(--tblr-tabler-fg) !important; }
   * }}}
  */
  def `text-tabler-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-teal {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-teal-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-teal-fg { color:var(--tblr-teal-fg) !important; }
   * }}}
  */
  def `text-teal-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-truncate {
   *   overflow:hidden;
   *   text-overflow:ellipsis;
   *   white-space:nowrap;
   * }
   * }}}
  */
  def `text-truncate`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-twitter {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-twitter-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-twitter-fg { color:var(--tblr-twitter-fg) !important; }
   * }}}
  */
  def `text-twitter-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-uppercase { text-transform:uppercase !important; }
   * }}}
  */
  def `text-uppercase`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-vimeo {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-vimeo-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-vimeo-fg { color:var(--tblr-vimeo-fg) !important; }
   * }}}
  */
  def `text-vimeo-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-vk {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-vk-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-vk-fg { color:var(--tblr-vk-fg) !important; }
   * }}}
  */
  def `text-vk-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-warning {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-warning-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-warning-emphasis { color:var(--tblr-warning-text-emphasis) !important; }
   * }}}
  */
  def `text-warning-emphasis`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-warning-fg { color:var(--tblr-warning-fg) !important; }
   * }}}
  */
  def `text-warning-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-white {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-white-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-white`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-white-50 {
   *   --tblr-text-opacity:1;
   *   color:rgba(255,255,255,.5) !important;
   * }
   * }}}
  */
  def `text-white-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-wrap { white-space:normal !important; }
   * }}}
  */
  def `text-wrap`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-xl-center { text-align:center !important; }
   * }}}
  */
  def `text-xl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-xl-end { text-align:right !important; }
   * }}}
  */
  def `text-xl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-xl-start { text-align:left !important; }
   * }}}
  */
  def `text-xl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-xxl-center { text-align:center !important; }
   * }}}
  */
  def `text-xxl-center`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-xxl-end { text-align:right !important; }
   * }}}
  */
  def `text-xxl-end`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-xxl-start { text-align:left !important; }
   * }}}
  */
  def `text-xxl-start`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-yellow {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-yellow-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-yellow-fg { color:var(--tblr-yellow-fg) !important; }
   * }}}
  */
  def `text-yellow-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-youtube {
   *   --tblr-text-opacity:1;
   *   color:rgba(var(--tblr-youtube-rgb),var(--tblr-text-opacity)) !important;
   * }
   * }}}
  */
  def `text-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .text-youtube-fg { color:var(--tblr-youtube-fg) !important; }
   * }}}
  */
  def `text-youtube-fg`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `theme-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .timeline {
   *   --tblr-timeline-icon-size:2.5rem;
   *   position:relative;
   *   list-style:none;
   *   padding:0;
   * }
   * }}}
  */
  def timeline: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .timeline-event { position:relative; }
   * }}}
  */
  def `timeline-event`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .timeline-event-card { margin-left:calc(var(--tblr-timeline-icon-size,2.5rem) + var(--tblr-page-padding)); }
   * }}}
  */
  def `timeline-event-card`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .timeline-event-icon {
   *   position:absolute;
   *   display:flex;
   *   align-items:center;
   *   justify-content:center;
   *   width:var(--tblr-timeline-icon-size,2.5rem);
   *   height:var(--tblr-timeline-icon-size,2.5rem);
   *   background:var(--tblr-gray-200);
   *   color:var(--tblr-secondary);
   *   border-radius:var(--tblr-border-radius);
   *   z-index:5;
   * }
   * }}}
  */
  def `timeline-event-icon`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .timeline-simple .timeline-event-icon { display:none; }
   * }}}
  */
  def `timeline-simple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast {
   *   --tblr-toast-zindex:1090;
   *   --tblr-toast-padding-x:0.75rem;
   *   --tblr-toast-padding-y:0.5rem;
   *   --tblr-toast-spacing:calc(var(--tblr-page-padding)*2);
   *   --tblr-toast-max-width:350px;
   *   --tblr-toast-font-size:0.875rem;
   *   --tblr-toast-bg:rgba(var(--tblr-body-bg-rgb),0.85);
   *   --tblr-toast-border-width:var(--tblr-border-width);
   *   --tblr-toast-border-color:var(--tblr-border-color);
   *   --tblr-toast-border-radius:var(--tblr-border-radius);
   *   --tblr-toast-box-shadow:var(--tblr-box-shadow);
   *   --tblr-toast-header-color:var(--tblr-secondary);
   *   --tblr-toast-header-bg:rgba(var(--tblr-body-bg-rgb),0.85);
   *   --tblr-toast-header-border-color:var(--tblr-border-color);
   *   width:var(--tblr-toast-max-width);
   *   max-width:100%;
   *   font-size:var(--tblr-toast-font-size);
   *   color:var(--tblr-toast-color);
   *   pointer-events:auto;
   *   background-color:var(--tblr-toast-bg);
   *   background-clip:padding-box;
   *   border:var(--tblr-toast-border-width) solid var(--tblr-toast-border-color);
   *   box-shadow:var(--tblr-toast-box-shadow);
   *   border-radius:var(--tblr-toast-border-radius);
   * }
   * }}}
  */
  def toast: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-azure { --tblr-toast-color:#4299e1; }
   * }}}
  */
  def `toast-azure`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-bitbucket { --tblr-toast-color:#0052cc; }
   * }}}
  */
  def `toast-bitbucket`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-blue { --tblr-toast-color:#0054a6; }
   * }}}
  */
  def `toast-blue`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-body {
   *   padding:var(--tblr-toast-padding-x);
   *   word-wrap:break-word;
   * }
   * }}}
  */
  def `toast-body`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-container {
   *   --tblr-toast-zindex:1090;
   *   position:absolute;
   *   z-index:var(--tblr-toast-zindex);
   *   width:-webkit-max-content;
   *   width:-moz-max-content;
   *   width:max-content;
   *   max-width:100%;
   *   pointer-events:none;
   * }
   * }}}
  */
  def `toast-container`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-cyan { --tblr-toast-color:#17a2b8; }
   * }}}
  */
  def `toast-cyan`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-danger { --tblr-toast-color:#d63939; }
   * }}}
  */
  def `toast-danger`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-dark { --tblr-toast-color:#182433; }
   * }}}
  */
  def `toast-dark`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-dribbble { --tblr-toast-color:#ea4c89; }
   * }}}
  */
  def `toast-dribbble`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-facebook { --tblr-toast-color:#1877f2; }
   * }}}
  */
  def `toast-facebook`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-flickr { --tblr-toast-color:#0063dc; }
   * }}}
  */
  def `toast-flickr`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-github { --tblr-toast-color:#181717; }
   * }}}
  */
  def `toast-github`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-google { --tblr-toast-color:#dc4e41; }
   * }}}
  */
  def `toast-google`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-green { --tblr-toast-color:#2fb344; }
   * }}}
  */
  def `toast-green`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-header {
   *   display:flex;
   *   align-items:center;
   *   padding:var(--tblr-toast-padding-y) var(--tblr-toast-padding-x);
   *   color:var(--tblr-toast-header-color);
   *   background-color:var(--tblr-toast-header-bg);
   *   background-clip:padding-box;
   *   border-bottom:var(--tblr-toast-border-width) solid var(--tblr-toast-header-border-color);
   *   border-top-left-radius:calc(var(--tblr-toast-border-radius) - var(--tblr-toast-border-width));
   *   border-top-right-radius:calc(var(--tblr-toast-border-radius) - var(--tblr-toast-border-width));
   * }
   * }}}
  */
  def `toast-header`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-indigo { --tblr-toast-color:#4263eb; }
   * }}}
  */
  def `toast-indigo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-info { --tblr-toast-color:#4299e1; }
   * }}}
  */
  def `toast-info`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-instagram { --tblr-toast-color:#e4405f; }
   * }}}
  */
  def `toast-instagram`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-light { --tblr-toast-color:#fcfdfe; }
   * }}}
  */
  def `toast-light`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-lime { --tblr-toast-color:#74b816; }
   * }}}
  */
  def `toast-lime`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-linkedin { --tblr-toast-color:#0a66c2; }
   * }}}
  */
  def `toast-linkedin`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-muted { --tblr-toast-color:#667382; }
   * }}}
  */
  def `toast-muted`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-orange { --tblr-toast-color:#f76707; }
   * }}}
  */
  def `toast-orange`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-pink { --tblr-toast-color:#d6336c; }
   * }}}
  */
  def `toast-pink`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-pinterest { --tblr-toast-color:#bd081c; }
   * }}}
  */
  def `toast-pinterest`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-primary { --tblr-toast-color:#0054a6; }
   * }}}
  */
  def `toast-primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-purple { --tblr-toast-color:#ae3ec9; }
   * }}}
  */
  def `toast-purple`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-red { --tblr-toast-color:#d63939; }
   * }}}
  */
  def `toast-red`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-rss { --tblr-toast-color:#ffa500; }
   * }}}
  */
  def `toast-rss`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-secondary { --tblr-toast-color:#667382; }
   * }}}
  */
  def `toast-secondary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-success { --tblr-toast-color:#2fb344; }
   * }}}
  */
  def `toast-success`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-tabler { --tblr-toast-color:#0054a6; }
   * }}}
  */
  def `toast-tabler`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-teal { --tblr-toast-color:#0ca678; }
   * }}}
  */
  def `toast-teal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-twitter { --tblr-toast-color:#1da1f2; }
   * }}}
  */
  def `toast-twitter`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-vimeo { --tblr-toast-color:#1ab7ea; }
   * }}}
  */
  def `toast-vimeo`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-vk { --tblr-toast-color:#6383a8; }
   * }}}
  */
  def `toast-vk`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-warning { --tblr-toast-color:#f76707; }
   * }}}
  */
  def `toast-warning`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-yellow { --tblr-toast-color:#f59f00; }
   * }}}
  */
  def `toast-yellow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toast-youtube { --tblr-toast-color:#ff0000; }
   * }}}
  */
  def `toast-youtube`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .toolbar {
   *   display:flex;
   *   flex-wrap:nowrap;
   *   flex-shrink:0;
   *   margin:0 -.5rem;
   * }
   * }}}
  */
  def toolbar: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tooltip {
   *   --tblr-tooltip-zindex:1080;
   *   --tblr-tooltip-max-width:200px;
   *   --tblr-tooltip-padding-x:var(--tblr-spacer-2);
   *   --tblr-tooltip-padding-y:var(--tblr-spacer-2);
   *   --tblr-tooltip-font-size:0.765625rem;
   *   --tblr-tooltip-color:var(--tblr-light);
   *   --tblr-tooltip-bg:var(--tblr-bg-surface-dark);
   *   --tblr-tooltip-border-radius:var(--tblr-border-radius);
   *   --tblr-tooltip-opacity:0.9;
   *   --tblr-tooltip-arrow-width:0.8rem;
   *   --tblr-tooltip-arrow-height:0.4rem;
   *   z-index:var(--tblr-tooltip-zindex);
   *   display:block;
   *   margin:var(--tblr-tooltip-margin);
   *   font-family:var(--tblr-font-sans-serif);
   *   font-style:normal;
   *   font-weight:400;
   *   line-height:1.4285714286;
   *   text-align:left;
   *   text-align:start;
   *   text-decoration:none;
   *   text-shadow:none;
   *   text-transform:none;
   *   letter-spacing:normal;
   *   word-break:normal;
   *   white-space:normal;
   *   word-spacing:normal;
   *   line-break:auto;
   *   font-size:var(--tblr-tooltip-font-size);
   *   word-wrap:break-word;
   *   opacity:0;
   * }
   * }}}
  */
  def tooltip: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tooltip .tooltip-arrow {
   *   display:block;
   *   width:var(--tblr-tooltip-arrow-width);
   *   height:var(--tblr-tooltip-arrow-height);
   * }
   * }}}
  */
  def `tooltip-arrow`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tooltip-inner {
   *   max-width:var(--tblr-tooltip-max-width);
   *   padding:var(--tblr-tooltip-padding-y) var(--tblr-tooltip-padding-x);
   *   color:var(--tblr-tooltip-color);
   *   text-align:center;
   *   background-color:var(--tblr-tooltip-bg);
   *   border-radius:var(--tblr-tooltip-border-radius);
   * }
   * }}}
  */
  def `tooltip-inner`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .top-0 { top:0 !important; }
   * }}}
  */
  def `top-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .top-100 { top:100% !important; }
   * }}}
  */
  def `top-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .top-50 { top:50% !important; }
   * }}}
  */
  def `top-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .touch .scrollable { overflow-y:auto !important; }
   * }}}
  */
  def touch: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox .tox-toolbar__primary { background:0 0 !important; }
   * }}}
  */
  def tox: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox:not(.tox-tinymce-inline) .tox-editor-header {
   *   border-bottom:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color) !important;
   *   box-shadow:none !important;
   *   padding:0 !important;
   * }
   * }}}
  */
  def `tox-editor-header`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox-statusbar { border-top:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color) !important; }
   * }}}
  */
  def `tox-statusbar`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox-tbtn { margin:0 !important; }
   * }}}
  */
  def `tox-tbtn`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox-tinymce {
   *   border:var(--tblr-border-width) var(--tblr-border-style) var(--tblr-border-color) !important;
   *   border-radius:4px !important;
   *   font-family:var(--tblr-font-sans-serif) !important;
   * }
   * }}}
  */
  def `tox-tinymce`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `tox-tinymce-inline`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox .tox-toolbar-overlord,
   * .tox:not(.tox-tinymce-inline) .tox-editor-header { background:0 0 !important; }
   * }}}
  */
  def `tox-toolbar-overlord`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox-toolbar__group { padding:0 .5rem 0; }
   * }}}
  */
  def `tox-toolbar__group`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .tox .tox-toolbar__primary { background:0 0 !important; }
   * }}}
  */
  def `tox-toolbar__primary`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tracking {
   *   --tblr-tracking-height:1.5rem;
   *   --tblr-tracking-gap-width:0.125rem;
   *   --tblr-tracking-block-border-radius:var(--tblr-border-radius);
   *   display:flex;
   *   gap:var(--tblr-tracking-gap-width);
   * }
   * }}}
  */
  def tracking: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tracking-squares .tracking-block { height:auto; }
   * }}}
  */
  def `tracking-block`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tracking-normal { letter-spacing:0 !important; }
   * }}}
  */
  def `tracking-normal`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tracking-squares { --tblr-tracking-block-border-radius:var(--tblr-border-radius-sm); }
   * }}}
  */
  def `tracking-squares`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tracking-tight { letter-spacing:-.05em !important; }
   * }}}
  */
  def `tracking-tight`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .tracking-wide { letter-spacing:.05em !important; }
   * }}}
  */
  def `tracking-wide`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .translate-middle { transform:translate(-50%,-50%) !important; }
   * }}}
  */
  def `translate-middle`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .translate-middle-x { transform:translateX(-50%) !important; }
   * }}}
  */
  def `translate-middle-x`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .translate-middle-y { transform:translateY(-50%) !important; }
   * }}}
  */
  def `translate-middle-y`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.single .ts-control,
   * .ts-wrapper.single .ts-control input { cursor:pointer; }
   * }}}
  */
  def `ts-control`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .plugin-dropdown_input.focus .ts-dropdown .dropdown-input {
   *   border-color:#80aad3;
   *   outline:0;
   *   box-shadow:var(--tblr-box-shadow-input),0 0 0 .25rem rgba(var(--tblr-primary-rgb),.25);
   * }
   * }}}
  */
  def `ts-dropdown`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-dropdown.plugin-optgroup_columns .ts-dropdown-content { display:flex; }
   * }}}
  */
  def `ts-dropdown-content`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-hidden-accessible {
   *   border:0 !important;
   *   clip:rect(0 0 0 0) !important;
   *   -webkit-clip-path:inset(50%) !important;
   *   clip-path:inset(50%) !important;
   *   overflow:hidden !important;
   *   padding:0 !important;
   *   position:absolute !important;
   *   width:1px !important;
   *   white-space:nowrap !important;
   * }
   * }}}
  */
  def `ts-hidden-accessible`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-input { color:inherit; }
   * }}}
  */
  def `ts-input`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.single .ts-control,
   * .ts-wrapper.single .ts-control input { cursor:pointer; }
   * }}}
  */
  def `ts-wrapper`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-drag_drop .ui-sortable-helper { box-shadow:0 2px 5px rgba(0,0,0,.2); }
   * }}}
  */
  def `ui-sortable-helper`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.plugin-drag_drop.multi>.ts-control>div.ui-sortable-placeholder {
   *   visibility:visible !important;
   *   background:#f2f2f2 !important;
   *   background:rgba(0,0,0,.06) !important;
   *   border:0 none !important;
   *   box-shadow:inset 0 0 12px 4px #fff;
   * }
   * }}}
  */
  def `ui-sortable-placeholder`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .user-select-all {
   *   -webkit-user-select:all !important;
   *   -moz-user-select:all !important;
   *   user-select:all !important;
   * }
   * }}}
  */
  def `user-select-all`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .user-select-auto {
   *   -webkit-user-select:auto !important;
   *   -moz-user-select:auto !important;
   *   -ms-user-select:auto !important;
   *   user-select:auto !important;
   * }
   * }}}
  */
  def `user-select-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .user-select-none {
   *   -webkit-user-select:none !important;
   *   -moz-user-select:none !important;
   *   -ms-user-select:none !important;
   *   user-select:none !important;
   * }
   * }}}
  */
  def `user-select-none`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css
   * {{{
   * .ts-wrapper.is-valid,
   * .was-validated .valid,
   * .was-validated :valid+.ts-wrapper { border-color:var(--tblr-form-valid-color); }
   * }}}
  */
  def valid: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `valid-feedback`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * 
   * }}}
  */
  def `valid-tooltip`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .vh-100 { height:100vh !important; }
   * }}}
  */
  def `vh-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .visible { visibility:visible !important; }
   * }}}
  */
  def visible: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .visually-hidden,
   * .visually-hidden-focusable:not(:focus):not(:focus-within) {
   *   width:1px !important;
   *   height:1px !important;
   *   padding:0 !important;
   *   margin:-1px !important;
   *   overflow:hidden !important;
   *   clip:rect(0,0,0,0) !important;
   *   white-space:nowrap !important;
   *   border:0 !important;
   * }
   * }}}
  */
  def `visually-hidden`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .visually-hidden,
   * .visually-hidden-focusable:not(:focus):not(:focus-within) {
   *   width:1px !important;
   *   height:1px !important;
   *   padding:0 !important;
   *   margin:-1px !important;
   *   overflow:hidden !important;
   *   clip:rect(0,0,0,0) !important;
   *   white-space:nowrap !important;
   *   border:0 !important;
   * }
   * }}}
  */
  def `visually-hidden-focusable`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .vr {
   *   display:inline-block;
   *   align-self:stretch;
   *   width:var(--tblr-border-width);
   *   min-height:1em;
   *   background-color:currentcolor;
   *   opacity:.16;
   * }
   * }}}
  */
  def vr: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .vstack {
   *   display:flex;
   *   flex:1 1 auto;
   *   flex-direction:column;
   *   align-self:stretch;
   * }
   * }}}
  */
  def vstack: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .vw-100 { width:100vw !important; }
   * }}}
  */
  def `vw-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-0 { width:0 !important; }
   * }}}
  */
  def `w-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-1 { width:.25rem !important; }
   * }}}
  */
  def `w-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-100 { width:100% !important; }
   * }}}
  */
  def `w-100`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-2 { width:.5rem !important; }
   * }}}
  */
  def `w-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-25 { width:25% !important; }
   * }}}
  */
  def `w-25`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-3 { width:1rem !important; }
   * }}}
  */
  def `w-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-33 { width:33.33333% !important; }
   * }}}
  */
  def `w-33`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-4 { width:1.5rem !important; }
   * }}}
  */
  def `w-4`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-5 { width:2rem !important; }
   * }}}
  */
  def `w-5`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-50 { width:50% !important; }
   * }}}
  */
  def `w-50`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-6 { width:3rem !important; }
   * }}}
  */
  def `w-6`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-66 { width:66.66666% !important; }
   * }}}
  */
  def `w-66`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-7 { width:5rem !important; }
   * }}}
  */
  def `w-7`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-75 { width:75% !important; }
   * }}}
  */
  def `w-75`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-8 { width:8rem !important; }
   * }}}
  */
  def `w-8`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-auto { width:auto !important; }
   * }}}
  */
  def `w-auto`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-full { width:100% !important; }
   * }}}
  */
  def `w-full`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .w-px { width:1px !important; }
   * }}}
  */
  def `w-px`: BtsClass = this

  /**
   * Files:tabler-vendors.min.css;tabler-vendors.rtl.min.css;tabler.min.css;tabler.rtl.min.css
   * {{{
   * .ts-wrapper.is-invalid,
   * .was-validated .invalid,
   * .was-validated :invalid+.ts-wrapper { border-color:var(--tblr-form-invalid-color); }
   * }}}
  */
  def `was-validated`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .z-0 { z-index:0 !important; }
   * }}}
  */
  def `z-0`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .z-1 { z-index:1 !important; }
   * }}}
  */
  def `z-1`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .z-2 { z-index:2 !important; }
   * }}}
  */
  def `z-2`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .z-3 { z-index:3 !important; }
   * }}}
  */
  def `z-3`: BtsClass = this

  /**
   * Files:tabler.min.css;tabler.rtl.min.css
   * {{{
   * .z-n1 { z-index:-1 !important; }
   * }}}
  */
  def `z-n1`: BtsClass = this


  /**
  * {{{
  *   important this css class 
  * }}}
  */
  def important(@unused styles: BtsClass): BtsClass = this
  /**
  * {{{
       for anything class inject
  * }}}
  */
  def raw(@unused classString: String): BtsClass = this
  
  /**
  * {{{
  *    css variant
  * }}}
  */
  def variant(selector: String, styles: BtsClass): BtsClass = this
}
