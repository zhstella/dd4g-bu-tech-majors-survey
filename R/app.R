library(shiny)
library(gt)
library(plotly)
library(ggplot2)


app <- function() {

  ui <- fluidPage(
    fluidRow(
    titlePanel("DEI in Tech Climate Survey Interactive Report"),
    tabsetPanel(
    tabPanel(
      "Welcome",
      mainPanel(
        h1("Welcome to the 2022 DEI in Tech Climate Survey Report"),
        p("This report is brought to you by the DEI Tech Collective.
        The Collective is an opportunity for BU tech and computing groups to unite around efforts to educate each other
        and address inequity issues within the community."),
        br(),
        p("To learn more about our initiative, visit the",
          a("DEI Tech Collective website.",
            href = "https://sites.bu.edu/dei-in-tech/")),
        hr(),
        h3("What is the DEI in Tech Climate Survey?"),
        p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc vitae metus eget lectus posuere facilisis. Ut tempor tempus consectetur. Fusce eu semper ligula. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Aliquam tempor, libero non mollis varius, ex ex imperdiet arcu, vitae porta felis diam id sapien. Sed facilisis est sed cursus tincidunt. Suspendisse hendrerit velit sed odio finibus varius."),
        h3("How was the survey created?"),
        p("Nam a risus eget est bibendum luctus ut at sem. Donec nec quam hendrerit, eleifend purus eget, posuere quam. Cras non ex vitae arcu pretium posuere ut tempor massa. Nullam vitae pellentesque nisl, eu hendrerit nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus."),
        h3("What can I do on this site?"),
        p("Donec est arcu, tristique a fringilla eu, mollis a diam. Duis fermentum aliquam quam vel finibus. Fusce vitae nibh nisl. Aenean ac purus est. Aenean in tincidunt nulla. Fusce maximus cursus enim, id blandit massa rhoncus vel. Duis ullamcorper in mi ut volutpat. Quisque sit amet luctus velit. Sed maximus tortor ipsum.")
      )
    ),
    tabPanel(
      "General Report",
      mainPanel(
        h1("General Report and Summaries"),
        p("Below is a general overview of the demographics of students who completed the survey and summaries detailing the common trends seen throughout the survey."),
        hr(),
        h3("Respondent Demographics"),
        p("Aliquam tempor, libero non mollis varius, ex ex imperdiet arcu, vitae porta felis diam id sapien. Sed facilisis est sed cursus tincidunt. Suspendisse hendrerit velit sed odio finibus varius."),
        plotOutput("demo_plot"),
        h3("Common Trends"),
        p("Nam a risus eget est bibendum luctus ut at sem. Donec nec quam hendrerit, eleifend purus eget, posuere quam. Cras non ex vitae arcu pretium posuere ut tempor massa. Nullam vitae pellentesque nisl, eu hendrerit nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus.")
      )
    ),
      tabPanel(
        "Build-a-Graph",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "qtype",
              label = "Select question type",
              choices = c("Agreement", "Adjectives", "Course Satisfaction", "Discrimination"),
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
              choices = c("gender", "race", "first_gen", "international", "none"),
              multiple = FALSE
            )
          ),
          mainPanel(
            plotOutput("freq_plot")
          )
        )
      )
      # compare_groups_ui("compare_groups")

    )),
    hr(),
    imageOutput("photo", height = "1%", width = "1%")
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
      if(input$qtype == "Discrimination")
        updateSelectInput(session, "question",
                          choices = unique(discrimination_q_tbl$selected_q),
                          label = "Select question"
        )
    })

    output$photo <- renderImage({
      list(
        src = "../techcollectivelogo.png",
        contentType = "image/png",
        height = "50px"
      )
    }, deleteFile = FALSE)

    output$freq_plot <- renderPlot(
      {
        var <- input$variable

        if (input$qtype == "Course Satisfaction") {
          cdf = course_ldf #cdf = current dataframe
          graphTitle = paste("Survey Prompt: How satisfied are you in regards to the instructional support\n(ex. support from professors) you've received in", input$question, "?")
        } else if (input$qtype == "Adjectives"){
          cdf = adj_ldf
          v <- strsplit(input$question, split = ":")
          graphTitle = paste("Survey Prompt: Please select one option between the following set of adjectives that best\nrepresents how you would rate your major department based on what you have seen and/or\nyour own personal experience: ", "1 = ", v[[1]], "and ", "5 = ", v[[1:2]])
        }else if (input$qtype == "Agreement"){
          cdf = agreement_ldf
          graphTitle = paste("Survey Prompt: Please indicate your level of agreement with the following statement:\n", input$question)
        }else if (input$qtype == "Discrimination"){
          cdf = dis_ldf
          graphTitle = paste("Survey Prompt:", input$question)
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
            ggplot(aes(x = response, fill = .data[[var]], group = .data[[var]])) %>%
            stack_freq_prop(title = graphTitle)
        }
      },
      height = 600
    )

    output$demo_plot <- renderPlot(
      {
        select_df <-
          race_ldf
        any_response <- nrow(select_df) > 0

        rdf <- select_df %>%
          count_prop_complete()

        rdf %>%
          ggplot(aes(x = response)) %>%
          stack_freq_prop(title = "Prompt: Please indicate all of the racial and/or ethnic groups you most identify with.")




        }

    )
    # compare_group_server("compare_groups")
  }

  shinyApp(ui = ui, server = server)
}

shiny::runApp(app())
