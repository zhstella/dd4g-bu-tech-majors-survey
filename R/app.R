library(shiny)
library(gt)


app <- function() {

  ui <- fluidPage(
    titlePanel("DEI in Tech Climate Survey Interactive Report"),
    tabsetPanel(tabPanel(
      "Welcome"
    ),
    tabPanel(
      "General Report"
    ),
      tabPanel(
        "Build-a-Graph",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "qtype",
              label = "Select question type",
              choices = c("Agreement", "Adjectives", "Course Satisfaction", "Department Satisfaction"),
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
              choices = c("gender", "race", "none"),
              multiple = FALSE
            )
          ),
          mainPanel(
            headerPanel("Course satisfication response distribution"),
            plotOutput("freq_plot")
          )
        )
      )
      # compare_groups_ui("compare_groups")
    ),
    hr(),
    print("~~~my disclaimer~~~~")
  )

  server <- function(input, output, session) {
    observe({
      if(input$qtype == "Agreement")
        updateSelectInput(session, "question",
                          choices = unique(agreement_q$question_text),
                          label = "Select agreement statement"
        )
    })
    observe({
      if(input$qtype == "Course Satisfaction")
        updateSelectInput(session, "question",
                          choices = unique(course_ldf$question_text),
                          label = "Select course",
        )
    })
    observe({
      if(input$qtype == "Adjectives")
        updateSelectInput(session, "question",
                          choices = unique(adjectives_q$question_text),
                          label = "Select adjectives to compare",
        )
    })
    observe({
      if(input$qtype == "Department Satisfaction")
        updateSelectInput(session, "question",
                          choices = unique(dep_tbl$selected_q),
                          label = "Select department"
        )
    })


    output$freq_plot <- renderPlot(
      {
        var <- input$variable

        if (input$qtype == "Course Satisfaction") {
          cdf = course_ldf #cdf = current dataframe
        } else if (input$qtype == "Adjectives"){
          cdf = adj_ldf
        }else if (input$qtype == "Agreement"){
          cdf = agreement_ldf
        }else if (input$qtype == "Department Satisfaction"){
          cdf = course_ldf
        }
        input <- list(selected_q = input$question)
        select_df <-
          cdf %>%
          filter(str_detect(question_text, input$selected_q))
        any_response <- nrow(select_df) > 0

        if (var == "none"){
          rdf <- select_df %>%
            count_prop_complete()

          rdf %>%
            ggplot(aes(x = response)) %>%
            stack_freq_prop(title = input$question)
        }else{
          rdf <- select_df %>%
            count_prop_complete(.data[[var]])

          rdf %>%
            ggplot(aes(x = response, fill = .data[[var]])) %>%
            stack_freq_prop(title = input$question)
        }
      },
      height = 600
    )
    # compare_group_server("compare_groups")
  }

  shinyApp(ui = ui, server = server)
}

shiny::runApp(app())
