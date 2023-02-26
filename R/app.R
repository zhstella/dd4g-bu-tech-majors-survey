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
              choices = c("agreement_q", "course_satisfaction_q", "adjectives_q", "department_q"),
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
                          choices = unique(agreement_q$question_text),
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
                          choices = unique(adjectives_q$question_text),
        )
    })
    observe({
      if(input$qtype == "department_q")
        updateSelectInput(session, "question",
                          choices = unique(dep_tbl$selected_q),
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
          stack_freq_prop(g, title = title)
        } else if (input$qtype == "adjectives_q"){
          input <- list(selected_q = input$question)
          select_df <-
            adj_ldf %>%
            filter(str_detect(question_text, input$selected_q))
          any_response <- nrow(select_df) > 0

            select_df %>%
            ggplot(aes(x = response)) +
            geom_bar() +
            scale_x_discrete(guide = guide_axis(n.dodge = 2), drop = FALSE)

        }else if (input$qtype == "agreement_q"){
          input <- list(selected_q = input$question)
          select_df <-
            agreement_ldf %>%
            filter(str_detect(question_text, input$selected_q))
          any_response <- nrow(select_df) > 0

          select_df %>%
            ggplot(aes(x = response)) +
            geom_bar() +
            scale_x_discrete(guide = guide_axis(n.dodge = 2), drop = FALSE)
        }else if (input$qtype == "department_q"){
          input <- list(selected_q = input$question)
          select_df <-
            course_ldf %>%
            filter(str_detect(question_text, input$selected_q))
          any_response <- nrow(select_df) > 0

          select_df %>%
            ggplot(aes(x = response)) +
            geom_bar() +
            scale_x_discrete(guide = guide_axis(n.dodge = 2), drop = FALSE)
        }

      },
      height = 600
    )
    # compare_group_server("compare_groups")
  }

  shinyApp(ui = ui, server = server)
}
