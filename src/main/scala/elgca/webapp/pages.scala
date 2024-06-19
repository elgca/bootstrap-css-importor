package elgca.webapp


sealed trait Page(val title: String)

case object HomePage extends Page("Home")

case object DashboardPage extends Page("Dashboard")

case object TodoListPage extends Page("Todo List")

case object NotFoundPage extends Page("Page Not Found")

