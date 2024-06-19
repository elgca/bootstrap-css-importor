package elgca.webapp

import com.raquo.laminar.api.L.{*, given}
import scalabootstrap.btsp
import scalabootstrap.css

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
object PageView extends View {

  lazy val view: HtmlElement = {
    div(
        cls := btsp.`page-wrapper-full`.css,
        div(
            cls := btsp.`page-header`.`d-print-none`.css,
        ),
        div(
            cls := btsp.`page-body`.`layout-fluid`.css,
            div(
                cls := btsp.`container-xl`.css,
                HomePageView.view,
            ).amend(
                (1 to 100).map(x => div(h1(s"haaaahaaa + ${x}"))): _*,
            ),
        ),
    )
  }

}
