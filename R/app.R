#' @export
app <- function(...) {

  ui <- fluidPage(
    titlePanel("DEI Student Survey"),
    tabsetPanel(
      course_satisfaction_ui("course_satisfaction"),
      course_corr_ui("course_corr"),
      compare_groups_ui("compare_groups")
    )
  )

    server <- function(input, output) {
    course_satisfaction_server("course_satisfaction")
    course_corr_server("course_corr")
    compare_group_server("compare_groups")
  }

  shinyApp(ui = ui, server = server)
}

