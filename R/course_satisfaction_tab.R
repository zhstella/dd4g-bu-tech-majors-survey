course_satisfaction_ui <- function(id) {
  tabPanel(
    "Course satisfication",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          NS(id, "course"),
          label = "Select courese(s)",
          choices = unique(course_df$course),
          multiple = TRUE
        )
      ),
      mainPanel(
        headerPanel("Course satisfication response distribution"),
        plotOutput(NS(id, "freq_plot"))
      )
    )
  )
}
course_satisfaction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$freq_plot <- renderPlot(
      {
        if (length(input$course) > 0) {
          g <- course_df %>%
            filter(course %in% input$course) %>%
            group_by(course) %>%
            count(response, name = "count", .drop = FALSE) %>%
            mutate(prop = count / sum(count)) %>%
            ggplot(aes(x = response, fill = course, group = course))
          title <- "Responses for selected course(s)"
        } else {
          g <- course_df %>%
            count(response, name = "count", .drop = FALSE) %>%
            mutate(prop = count / sum(count)) %>%
            ggplot(aes(x = response, group = factor(1)))
          title <- "All responses"
        }
        stack_freq_prop(g, title = title)
      },
      height = 600
    )
  })
}

