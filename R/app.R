library(shiny)
library(gt)

source(here::here("R/source_all.R"))

app <- function() {

  ui <- fluidPage(
    titlePanel("DEI Student Survey"),
    tabsetPanel(
      tabPanel(
        "Course satisfication",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "course",
              label = "Select courese(s)",
              choices = unique(course_df$course),
              multiple = TRUE
            )
          ),
          mainPanel(
            headerPanel("Course satisfication response distribution"),
            plotOutput("freq_plot")
          )
        )
      )
      # compare_groups_ui("compare_groups")
    )
  )

  server <- function(input, output) {
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
    # compare_group_server("compare_groups")
  }

  shinyApp(ui = ui, server = server)
}

