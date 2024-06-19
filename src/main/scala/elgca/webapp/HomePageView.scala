package elgca.webapp

import elgca.webapp.View
import com.raquo.laminar.api.L.{*, given}

object HomePageView extends View {
  def view: HtmlElement = div(
      h1("Hello, World"),
      h2("This is HomePage"),
      h3("HAAHAHAHA"),
  )
}
