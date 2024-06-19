package elgca.webapp

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.codecs.*
import org.scalajs.dom
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import org.scalajs.dom.Event
import org.scalajs.dom.document
import org.scalajs.dom.HTMLSelectElement
import org.scalajs.dom.html

import scalabootstrap.{*, given}
import com.raquo.laminar.keys.HtmlAttr
import com.raquo.laminar.DomApi
import elgca.webapp.extops.dyattr

/**
 * Main
 */
object App {
  @JSImport("@find/**/App.css")
  @js.native
  private def importStyle(): Unit = js.native

  def main(args: Array[String]): Unit = {
    lazy val appElement = {
//      println(btsp.test-)

      // test-demo load from App.css
      println(btsp.test2.css)
      println(btsp.`test-demo`.css)
      div(
          PageNavBarView.view,
          PageView.view,
      )
    }

    lazy val container = dom.document.body

    render(container, appElement)
  }
}
