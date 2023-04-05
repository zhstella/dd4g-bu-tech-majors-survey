library(shiny)
library(gt)
library(plotly)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(shinyalert)



app <- function() {

  ui <- fluidPage(
    useShinyjs(),
    fluidRow(
    titlePanel("DEI in Tech Climate Survey Interactive Report"),
    tabsetPanel(id = "inTabset",
    tabPanel(
      "Welcome", value = "panel1",
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
        p("The purpose of this survey was to assess the climate of tech departments at Boston University. This survey was intended for students in Computer Science, Math and Stats, Computing and Data Science, Information Systems, and Computer Engineering. The survey was modeled after already existing climate surveys from various institutions, including the University of Michigan Computer Science and Engineering Climate, Diversity, Equity, and Inclusion Assessment.
"),
        h3("What can I do on this site?"),
        p("Navigate to the “Survey Report” tab to view the Climate Survey Report. Navigate to the “Build-a-Graph” tab if you’d like to interact with the data yourself. Here you can select specific questions from the survey and see how students responded. "),

        h3(id = "how", "How do I use Build-a-Graph?"),
        strong("How to Build a Graph"),
        tags$ol(
          tags$li("Select a section or question type from the survey"),
          tags$li("Choose the particular question of interest"),
          tags$li("To view breakdowns of the selected question by demographic attributes, identify the variable of interest")
        ),
        strong("How to Read the Graphs"),
        tags$ol(
          tags$li("The top graph, ‘count’, displays the total number of students that selected each response. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of female students, male students, etc. that selected each response."),
          tags$li("The bottom graph, ‘prop’, displays the total number of students that selected each response, in proportion to the total number of students that responded to the question. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of female students, male students, etc. that selected each response, in proportion to the total number of female students, male students etc. that responded to the question."),
          tags$li("Example: The graph below is the data for the question, 'Have you ever experienced discrimination or disrespectful/inappropriate behavior in your major department?' and the gender variable is selected.
In the top graph, when looking at the values for the 'No' response, female has a value of 110. This means 110 female students responded No. In the bottom graph, when looking at the values for the 'No' response, female has a value of 64%. This means 64% of all female students responded No."),
           )
        ),
      imageOutput("photo1", height = "50%", width = "50%"),
    ),
    tabPanel(
      "Survey Report",
      mainPanel(
        h1("Survey Report"),
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
              label = tags$span(
                "Select question type", bsButton("q", label = "", icon = icon("info"), style = "info", size = "extra-small")),
              choices = c("Agreement", "Adjectives", "Course Satisfaction", "Discrimination"),
              multiple = FALSE,
              selected = "Agreement"
            ),  bsPopover(
              id = "q",
              title = "More information",
              content = paste0(
                "Select a section or question type from the survey."
              ),
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            ),
            selectInput(
              "dep",
              label = "Filter by course or by department?",
              choices = c("Course", "Department"),
              multiple = FALSE
            ),
            selectInput(
              "question",
              label = "Select question",
              choices = NULL,
              multiple = FALSE
            ),
            selectInput(
              "variable",
              label = tags$span(
                "Select variable", bsButton("v", label = "", icon = icon("info"), style = "info", size = "extra-small")),
              choices = c("gender", "race", "first_gen", "international", "major", "prepared", "none"),
              multiple = FALSE,
            ),  bsPopover(
              id = "v",
              title = "More information",
              content = paste0(
                "To view breakdowns of the selected question by demographic attributes, identify the variable of interest."
              ),
              placement = "right",
              trigger = "hover",
              options = list(container = "body")
            ),
            p("Need help with using the tool or interpreting the graphs?"),
            actionButton("help", "Help")

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
    observeEvent(input$help, {
      #updateTabsetPanel(session, "inTabset",
             #           selected = "panel1")
      shinyalert("How to read the graphs", "1.
The top graph, ‘count’, displays the total number of students that selected each response. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of female students, male students, etc. that selected each response.
2.
The bottom graph, ‘prop’, displays the total number of students that selected each response, in proportion to the total number of students that responded to the question. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of female students, male students, etc. that selected each response, in proportion to the total number of female students, male students etc. that responded to the question.
3. Example
For example, let’s say we’re looking at the question, 'Have you ever experienced discrimination or disrespectful/inappropriate behavior in your major department?' and the gender variable is selected.
In the top graph, when looking at the values for the 'No' response, female has a value of 110. This means 110 female students responded No. In the bottom graph, when looking at the values for the 'No' response, female has a value of 64%. This means 64% of all female students responded No.

Return to the Welcome tab and scroll down to the, 'How do I use Build-a-Graph?' section to see an example with a graph.

")

    })

    observe({
      if(input$qtype == "Agreement"){
        updateSelectInput(session, "question",
                          choices = unique(agreement_q$question_text),
                          label = "Select agreement statement"

        )
      }
      if(input$qtype == "Course Satisfaction"){
        updateSelectInput(session, "question",
                          choices = unique(course_ldf$question_text),
                          label = "Select course",
                          shinyjs::show("dep")
        )
      }
      if(input$qtype == "Course Satisfaction" & input$dep == "Department"){
        updateSelectInput(session, "question",
                          choices = unique(dep_tbl$selected_q),
                          label = "Select department"
        )
      }
      if(input$qtype == "Course Satisfaction" & input$dep == "Course"){
        updateSelectInput(session, "question",
                          choices = unique(course_ldf$question_text),
                          label = "Select course"
        )
      }
      if(input$qtype == "Adjectives"){
        updateSelectInput(session, "question",
                          choices = unique(adjectives_q$question_text),
                          label = "Select adjectives to compare",
        )
      }
      if(input$qtype == "Discrimination"){
        updateSelectInput(session, "question",
                          choices = unique(discrimination_q_tbl$selected_q),
                          label = "Select question"
        )
      }
      if(input$qtype != "Course Satisfaction")
        shinyjs::hide("dep")
    })


    output$photo <- renderImage({
      list(
        src = "../techcollectivelogo.png",
        contentType = "image/png",
        height = "50px"
      )
    }, deleteFile = FALSE)

    output$photo1 <- renderImage({
      list(
        src = "../example.png",
        contentType = "image/png",
        height = "450px"
      )
    }, deleteFile = FALSE)

    output$freq_plot <- renderPlot(
      {
        var <- input$variable

        if (input$qtype == "Course Satisfaction" & input$dep == "Course") {
          cdf = course_ldf #cdf = current dataframe
          graphTitle = paste("Survey Prompt: How satisfied are you in regards to the instructional support\n(ex. support from professors) you've received in", input$question, "?")
        } else if (input$qtype == "Adjectives"){
          cdf = adj_ldf
          adj1 <- unlist(strsplit(input$question, split = ":"))
          graphTitle = paste("Survey Prompt: Please select one option between the following set of adjectives that best\nrepresents how you would rate your major department based on what you have seen and/or\nyour own personal experience: ", "1 = ", adj1, "and ", "5 = ", substring(input$question, nchar(adj1)+2, nchar(input$question)))
        }else if (input$qtype == "Agreement"){
          cdf = agreement_ldf
          graphTitle = paste("Survey Prompt: Please indicate your level of agreement with the following statement:\n", input$question)
        }else if (input$qtype == "Discrimination"){
          cdf = dis_ldf
          graphTitle = paste("Survey Prompt:", input$question)
        }else if (input$qtype == "Course Satisfaction" & input$dep == "Department"){
          cdf = course_ldf
          graphTitle = paste("This data represents the overall course satisfaction for all",input$question, "courses.")
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
            stack_freq_prop(title = graphTitle)
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
