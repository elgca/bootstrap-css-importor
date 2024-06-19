package elgca.webapp

import com.raquo.laminar.api.L.{*, given}
import com.raquo.laminar.api.L
import scalabootstrap.*
import elgca.webapp.extops.{*, given}
import com.raquo.laminar.shoelace.sl.Icon

import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js
import com.raquo.airstream.status.Status
import com.raquo.laminar.shoelace.sl

object PageNavBarView extends View {
  def navigateTo(page: Page) = {
    href := "#"
  }
  private def pageLink(
      page: Page, caption: Option[String] = None,
  ): HtmlElement = {
    li(
        cls := btsp.`nav-link`.css,
        a(
            href := "#",
            span(
                cls := btsp.`nav-link-icon`.`d-md-none`.`d-lg-inline-block`.css,
                Icon.of(
                    _.name := "star-fill",
                ),
            ),
            span(
                cls := btsp.`text-white-50`.css,
                caption.getOrElse(page.title),
            ),
        ),
    )
  }

  val navMenu =
    ul(
        cls := btsp.`navbar-nav`.css,
        pageLink(HomePage),
        pageLink(DashboardPage),
        pageLink(TodoListPage),
        li(
            cls := btsp.`nav-item`.dropdown.css,
            a(
                cls := btsp.`nav-link`.`dropdown-toggle`.css,
                href := "#navbar-base",
                dyattr.data.bs.toggle._attr := "dropdown",
                dyattr.data.bs.auto.close._attr := "outside",
                role := "button",
                aria.expanded := false,
                span(
                    cls := btsp.`nav-link-icon`.`d-md-none`.`d-lg-inline-block`.css,
                ),
                span(
                    cls := btsp.`text-white-50`.css,
                    "dropdown",
                ),
            ),
            div(
                cls := btsp.`dropdown-menu`.css,
                div(
                    cls := btsp.`dropdown-menu-columns`.css,
                    div(
                        cls := btsp.`dropdown-menu-column`.css,
                    ).amend(
                        (1 to 3).map(x => {
                          a(
                              cls := btsp.`dropdown-item`.css,
                              navigateTo(HomePage),
                              HomePage.title + x,
                          )
                        }): _*,
                    ).amend(
                        div(
                            cls := btsp.dropend.css,
                            a(
                                cls := btsp.`dropdown-item`.`dropdown-toggle`.css,
                                href := "#sidebar-authentication",
                                dyattr.data.bs.toggle._attr := "dropdown",
                                dyattr.data.bs.auto.close._attr := "outside",
                                role := "button",
                                aria.expanded := false,
                                "Subscription",
                            ),
                            div(
                                cls := btsp.`dropdown-menu`.css,
                            ).amend(
                                (1 to 5).map(x =>
                                  a(
                                      cls := btsp.`dropdown-item`.css,
                                      navigateTo(TodoListPage),
                                      TodoListPage.title + x,
                                  )): _*,
                            ),
                        ),
                    ).amend(
                        a(
                            cls := btsp.`dropdown-item`.css,
                            navigateTo(HomePage),
                            "Test",
                        ),
                    ),
                    div(
                        cls := btsp.`dropdown-menu-column`.css,
                        a(
                            cls := btsp.`dropdown-item`.css,
                            navigateTo(DashboardPage),
                            "Col2",
                        ),
                        a(
                            cls := btsp.`dropdown-item`.css,
                            navigateTo(DashboardPage),
                            "Col3",
                            span(
                                cls := btsp.badge.`bg-green-lt`.`text-uppercase`.`ms-auto`.css,
                                "new",
                            ),
                        ),
                    ),
                ),
            ),
        ),
        pageLink(TodoListPage),
        pageLink(TodoListPage, "jiwel"),
        li(
            cls := btsp.`nav-item`.active.css,
            a(
                navigateTo(HomePage),
                cls := btsp.`nav-link`.css,
                span(
                    cls := btsp.`nav-link-icon`.`d-md-none`.`d-lg-inline-block`.css,
                    sl.Icon.of(
                        _.name("gear-fill"),
                        _ =>
                          fontSize.em(
                              1.3), // this is how you set icon size in shoelace
                    ),
                ),
                span("Email templates"),
            ),
        ),
        // pageLink(TodoListPage, "help"),
        pageLink(TodoListPage, "cssifd"),
    )
  def view: HtmlElement = {
    div(
        cls := btsp.`sticky-top`.`layout-fluid`.css,
        view2,
    )
  }

  def view2: HtmlElement = {
    div(
        cls := btsp.navbar.`navbar-expand-md`.`d-print-none`.css,
        div(
            cls := btsp.`container-xl`.css,
            button(
                // 移动模式下导航栏的空间合并为一个弹窗按钮
                cls := btsp.`navbar-toggler`.css,
                typ := "button",
                dyattr.data.bs.toggle._attr := "collapse",
                dyattr.data.bs.target._attr := "#navbar-menu",
                aria.controls := "navbar-menu",
                aria.expanded := false,
                aria.label := "Toggle navigation",
                span(cls := btsp.`navbar-toggler-icon`.css),
                // "data-bs-toggle".
            ),
            h1(
                // 添加左侧logo和标签名称
                cls := btsp.`navbar-brand`.`navbar-brand-autodark`.`pe-0`.`pe-md-3`.css,
                a(
                    navigateTo(HomePage),
                    img(
                        cls := btsp.`navbar-brand-image`.css,
                        src := "/static/logo.svg",
                        width := "110",
                        height := "32",
                        alt := "ViewALT", // 如果用户由于某种原因无法查看图像，alt 属性会提供图像的替代信息。
                    ),
                ),
            ),
            div(
                // 这里是右侧操作信息，用户头像、通知、设置等
                cls := btsp.`navbar-nav`.`flex-row`.`order-md-last`.css,
                div(
                    cls := btsp.`d-none`.`d-md-flex`.css,
                    a(
                        href := "#",
                        cls := btsp.`nav-link`.`px-0`.`hide-theme-dark`.css,
                        title := "Enable dark mode",
                        dyattr.data.bs.toggle._attr := "tooltip",
                        dyattr.data.bs.placement._attr := "button",
                        // {
                        //   val value =
                        //     svg.svg()
                        //   value.ref.innerHTML =
                        //     """<svg xmlns="http://www.w3.org/2000/svg" class="icon" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M12 3c.132 0 .263 0 .393 0a7.5 7.5 0 0 0 7.92 12.446a9 9 0 1 1 -8.313 -12.454z" /></svg>"""
                        //   value
                        // },
                        sl.Icon(
                            _.name := "brightness-high",
                            fontSize.rem(1.2),
                            _.title := "Enable light mode",
                            alt := "light",
                        ),
                    ),
                    div(
                        // 通知
                        cls := btsp.`nav-item`.dropdown.`d-none`.`d-md-flex`.`me-3`.css,
                        // shoelace 按钮版本
                        sl.Button(
                            cls := btsp.`nav-link`.`px-0`.css,
                            dyattr.data.bs.toggle._attr := "dropdown",
                            border := "none",
                            background := "none",
                            color := "transparent",
                            _.title := "Show notifications",
                            _.slots.prefix(
                                sl.Icon(
                                    _.name := "bell",
                                    // fontSize.rem(1.2),
                                    color.white,
                                ),
                            ),
                            span(cls := btsp.badge.`bg-red`.css),
                        ),
                        // ),
                        div(
                            // 这里是通知弹窗
                            cls := btsp.`dropdown-menu`.`dropdown-menu-arrow`.`dropdown-menu-end`.`dropdown-menu-card`.css,
                            div(
                                cls := btsp.card.css,
                                div(
                                    // 标题
                                    cls := btsp.`card-header`.css,
                                    h3("通知列表", cls := btsp.`card-title`.css),
                                ),
                                div(
                                    // 消息部分
                                    cls := btsp.`list-group`.`list-group-flush`.`list-group-hoverable`.css,
                                    div(
                                        // 一个消息列
                                        cls := btsp.`list-group-item`.css,
                                        div(
                                            // 消息中心
                                            cls := btsp.row.`align-items-center`.css,
                                            div(
                                                // 消息前面的小红点
                                                cls := btsp.`col-auto`.css,
                                                span(
                                                    cls := btsp.`status-dot`.`status-dot-animated`.`bg-red`.`d-block`.css,
                                                ),
                                            ),
                                            div(
                                                cls := btsp.col.`text-truncate`.css,
                                                a(
                                                    href := "#",
                                                    cls := btsp.`text-body`.`d-block`.css,
                                                    "Example 1",
                                                ),
                                                div(
                                                    cls := btsp.`d-block`.`text-secondary`.`text-truncate`.css,
                                                    "Change deprecated html tags to text decoration classes (#29604)",
                                                ),
                                            ),
                                            div(
                                                cls := btsp.`col-auto`.css,
                                                a(
                                                    href := "#",
                                                    cls := btsp.`list-group-item-action`.css,
                                                    sl.Icon.of(
                                                        _.name := "star",
                                                        _ => fontSize.rem(1.2),
                                                        _ => color.yellow,
                                                    ),
                                                ),
                                            ),
                                        ),
                                    ), {
                                      // 一个消息列, 来自模板的示例
                                      val v = div()
                                      v.ref.innerHTML = """
                      <div class="list-group-item">
                        <div class="row align-items-center">
                          <div class="col-auto"><span class="status-dot d-block"></span></div>
                          <div class="col text-truncate">
                            <a href="#" class="text-body d-block">Example 2</a>
                            <div class="d-block text-secondary text-truncate mt-n1">
                              justify-content:between ⇒ justify-content:space-between (#29734)
                            </div>
                          </div>
                          <div class="col-auto">
                            <a href="#" class="list-group-item-actions show">
                              <!-- Download SVG icon from http://tabler-icons.io/i/star -->
                              <svg xmlns="http://www.w3.org/2000/svg" class="icon text-yellow" width="24" height="24" viewBox="0 0 24 24" stroke-width="2" stroke="currentColor" fill="none" stroke-linecap="round" stroke-linejoin="round"><path stroke="none" d="M0 0h24v24H0z" fill="none"/><path d="M12 17.75l-6.172 3.245l1.179 -6.873l-5 -4.867l6.9 -1l3.086 -6.253l3.086 6.253l6.9 1l-5 4.867l1.179 6.873z" /></svg>
                            </a>
                          </div>
                        </div>
                      </div>"""
                                      v
                                    },
                                ),
                            ),
                        ),
                    ),
                ),
                div(
                    cls := btsp.`nav-item`.dropdown.css,
                    a(
                        // 这些都在a标签内，可以出发点击访问时间
                        href := "#",
                        cls := btsp.`nav-link`.`d-flex`.`lh-1`.`text-reset`.`p-0`.css,
                        dyattr.data.bs.toggle._attr := "dropdown",
                        aria.label := "Open user menu",
                        // sl.Avatar(
                        // cls := btsp.avatar.`avatar-sm`.css,
                        // _.image := "/static/avatars/010f.jpg",
                        // ),
                        span(
                            // 这里添加的是用户头像
                            cls := btsp.avatar.`avatar-sm`.css,
                            backgroundImage := "url(/static/avatars/010f.jpg)",
                        ),
                        div(
                            cls := btsp.`d-none`.`d-xl-block`.`ps-2`.css,
                            div("UserName"),
                            div(
                                cls := btsp.`mt-1`.small.`text-secondary`.css,
                                "Title",
                            ),
                        ),
                    ),
                    // 这里添加点击用户后的弹窗操作框
                    div(
                        cls := btsp.`dropdown-menu`.`dropdown-menu-end`.`dropdown-menu-arrow`.css,
                        // dyattr.data.bs.theme._attr := "light",
                        a(cls := btsp.`dropdown-item`.css, href := "#",
                            "Status"),
                        a(cls := btsp.`dropdown-item`.css, href := "#",
                            "Profile"),
                        a(cls := btsp.`dropdown-item`.css, href := "#",
                            "Feedback"),
                        div(cls := btsp.`dropdown-divider`.css), // 分割线
                        a(cls := btsp.`dropdown-item`.css, href := "#",
                            "Settings"),
                        a(cls := btsp.`dropdown-item`.css, href := "#",
                            "Logout"),
                    ),
                ),
            ),
            div(
                // 这里是菜单列表
                cls := btsp.collapse.`navbar-collapse`.css,
                idAttr := "navbar-menu",
                div(
                    cls := btsp.`d-flex`.`flex-column`.`flex-md-row`.`flex-fill`.`align-items-stretch`.`align-items-md-center`.css,
                    navMenu,
                ),
                // 加搜索框？
                div(
                    cls := btsp.`my-2`.`my-md-0`.`flex-grow-0`.`order-first`.`order-md-last`.css,
                    form(
                        div(
                            cls := btsp.`input-icon`.css,
                            span(
                                cls := btsp.`input-icon-addon`.css,
                                sl.Icon(
                                    _.name := "search",
                                ),
                            ),
                            input(
                                typ := "text",
                                value := "",
                                placeholder := "Search…",
                                aria.label := "Search in website",
                                cls := btsp.`form-control`.css,
                            ),
                        ),
                    ),
                ),
            ),
        ),
    )
  }

}
