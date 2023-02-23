library(shiny)
library(gt)


app <- function() {

  ui <- fluidPage(
    titlePanel("DEI Student Survey"),
    tabsetPanel(
      tabPanel(
        "Course satisfication",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "qtype",
              label = "Select question type",
              choices = c("agreement_q", "course_satisfaction_q", "adjectives_q"),
              multiple = FALSE
            ), selectInput(
              "question",
              label = "Select question",
              choices = NULL,
              multiple = FALSE
            ),
            selectInput(
              "variable",
              label = "Select variable",
              choices = c(unique(gender_df$gender), unique(race_df$race_str)),
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

  server <- function(input, output, session) {
    observe({
      if(input$qtype == "agreement_q")
        updateSelectInput(session, "question",
                          choices = unique(agreement_q$question_id),
        )
    })
    observe({
      if(input$qtype == "course_satisfaction_q")
        updateSelectInput(session, "question",
                          choices = unique(course_df$course),
        )
    })
    observe({
      if(input$qtype == "adjectives_q")
        updateSelectInput(session, "question",
                          choices = unique(adjectives_q$question_id),
        )
    })

    output$freq_plot <- renderPlot(
      {
        if (input$qtype == "course_satisfaction_q") {
          g <- course_df %>%
            filter(course %in% input$question) %>%
            group_by(course) %>%
            count(response, name = "count", .drop = FALSE) %>%
            mutate(prop = count / sum(count)) %>%
            ggplot(aes(x = response, fill = course, group = course))
          title <- "Responses for selected course(s)"
        } else if (input$qtype == "adjectives_q"){
          g <- adj_ldf %>%
            filter(input$question) %>%
            group_by(input$question) %>%
            count(response, name = "count", .drop = FALSE) %>%
            mutate(prop = count / sum(count)) %>%
            ggplot(aes(x = response, fill = input$question, group = course))
          title <- "Responses for selected course(s)"
        }else{
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
