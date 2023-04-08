library(shiny)
library(ggplot2)
library(shinyjs)
library(shinyBS)
library(shinyalert)


source("setup_data_frames.R")


variable_options <- c("gender", "race", "first_gen", "international", "major", "prepared", "none")
names(variable_options) <- c("Gender", "Race", "First Gen", "International", "Major", "Preparedness", "None")

ui <- fluidPage(
  useShinyjs(),
  fluidRow(
  titlePanel("DEI in Tech Climate Survey Interactive Report"),
  tabsetPanel(id = "inTabset",
  tabPanel(
    "Welcome", value = "panel1",
    mainPanel(style = "margin-left: 13%; margin-right: 15%; width: 75%",
      h1(style = "text-align: center", "Welcome to the 2022 DEI in Tech Climate Survey Report"),
      p("This report is brought to you by the DEI Tech Collective.
      The Collective is an opportunity for BU tech and computing groups to unite around efforts to educate each other
      and address inequity issues within the community."),
      br(),
      p("To learn more about our initiative, visit the",
        a("DEI Tech Collective website.",
          href = "https://sites.bu.edu/dei-in-tech/")),
      hr(),
      h3("What is the DEI in Tech Climate Survey?"),
      p("	The survey was administered in the Spring of 2022 by the BU DEI Tech Collective. The purpose of this survey was to assess the climate of tech departments at Boston University and begin to fill that data gap. This survey was intended for students in Computer Science, Math and Stats, Computing and Data Science, Information Systems, and Computer Engineering. The survey was modeled after already existing climate surveys from various institutions, including the University of Michigan Computer Science and Engineering Climate, Diversity, Equity, and Inclusion Assessment and assessments from the Computing Research Association.

"),
      h3("What can I do on this site?"),
      p("Navigate to the “Survey Report” tab to view the entire Climate Survey Report. This page includes:"),
      tags$li("Additional survey background details"),
      tags$li("BU population data overview"),
      tags$li("Dispaly of respondent demographics"),
      tags$li("Explanations of survey sections and accompanying trends"),
      tags$li("Disclaimers and challenges to be aware of"),
      p("\n"),
      p("Navigate to the “Build-a-Graph” tab to interact with the data yourself. Here you can select specific questions from the survey and filter by variables to better understand
      how different types of students responded. Information on the “General Report” tab will help with the interpretation of the data results that you compile."),

      h3(id = "how", "How do I use Build-a-Graph?"),
      strong("How to Build a Graph"),
      tags$ol(
        tags$li("Select a section or question type from the survey"),
        tags$li("Choose the particular question of interest"),
        tags$li("To view breakdowns of the selected question by demographic attributes, identify the variable of interest")
      ),
      strong("How to Read the Graphs"),
      tags$ol(
        tags$li("When a variable is not selected (or 'none' selected), the top graph, ‘count’, displays the total number of students that selected each response. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of self-identified female students, self-identified male students, etc. that selected each response."),
        tags$li("When a variable is not selected (or 'none' selected), the bottom graph, ‘prop’, displays the total number of students that selected each response, in proportion to the total number of students that responded to the question. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of self-identified-female students, self-identified male students, etc. that selected each response, in proportion to the total number of self-identified female students, self-identified male students etc. that responded to the question."),
         ),
      strong("Example: "),
      p("The graph below is the data for the question, 'Have you ever experienced discrimination or disrespectful/inappropriate behavior in your major department?' and the gender variable is selected.
In the top graph, when looking at the values for the 'No' response, female (the red bar) has a value of 110. This means 110 self-identified female students responded No. In the bottom graph, when looking at the values for the 'No' response, female (the red bar) has a value of 64%. This means 64% of all self-identified female students responded No."),
      imageOutput("photo1", height = "50%", width = "50%")),



  ),
  tabPanel(
    "Survey Report",
    mainPanel(style = "margin-left: 13%; margin-right: 15%; width: 75%",
      h1("Survey Report"),
      p("Thank you for your interest in the DEI in Tech Student Climate Survey. This campaign was motivated by the need to better understand student experiences in tech related departments at BU in an effort to inform areas for change and improvement. Please read on to learn more.
"),
      hr(),
      h3("Background"),
      p("Aliquam tempor, libero non mollis varius, ex ex imperdiet arcu, vitae porta felis diam id sapien. Sed facilisis est sed cursus tincidunt. Suspendisse hendrerit velit sed odio finibus varius."),
      plotOutput("demo_plot"),
      h3("Objectives"),
      p("Nam a risus eget est bibendum luctus ut at sem. Donec nec quam hendrerit, eleifend purus eget, posuere quam. Cras non ex vitae arcu pretium posuere ut tempor massa. Nullam vitae pellentesque nisl, eu hendrerit nisl. Interdum et malesuada fames ac ante ipsum primis in faucibus."),
      h3("Administering the Survey"),
      p("Nam a risus eget est bibendum luctus ut at sem. Donec nec quam hendrerit, eleifend purus eget, posuere quam. Cras non ex vitae arcu pretium posuer"),
      h3("Results"),
      p("Nam a risus eget est bibendum luctus ut at sem. Donec nec quam hendrerit, eleifend purus eget, posuere quam. Cras non ex vitae arcu pretium posuer"),
      h3("Conclusion"),
      p("Nam a risus eget est bibendum luctus ut at sem. Donec nec quam hendrerit, eleifend purus eget, posuere quam. Cras non ex vitae arcu pretium posuer"),
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
          ),selectInput(
            "dep",
            label = tags$span(
              "Filter by course or by department?", bsButton("depa", label = "", icon = icon("info"), style = "info", size = "extra-small")),
            choices = c("Course", "Department"),
            multiple = FALSE,
          ), bsPopover(
            id = "depa",
            title = "More information",
            content = paste0(
              "View the course satisfaction for individual classes or view the total course satisfaction for courses within a department."
            ),
            placement = "right",
            trigger = "hover",
            options = list(container = "body")
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
            choices = variable_options,
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
          p("Need help with using reading the graphs?"),
          actionButton("help", "Help")

        ),
        mainPanel(
          plotOutput("freq_plot")
        )
      )
    )

  )),
  hr(),
  imageOutput("photo", height = "1%", width = "1%")

)


server <- function(input, output, session) {
  observeEvent(input$help, {
    #updateTabsetPanel(session, "inTabset",
           #           selected = "panel1")
    shinyalert("How to read the graphs", "1. Top Graph
When a variable is not selected (or 'none' selected), the top graph, ‘count’, displays the total number of students that selected each response. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of self-identified female students, self-identified male students, etc. that selected each response.
2. Bottom Graph
When a variable is not selected (or 'none' selected), the bottom graph, ‘prop’, displays the total number of students that selected each response, in proportion to the total number of students that responded to the question. When a variable is selected, the responses will be broken down by the selected variable. This means if the variable ‘gender’ is selected, the values in this graph will show the number of self-identified-female students, self-identified male students, etc. that selected each response, in proportion to the total number of self-identified female students, self-identified male students etc. that responded to the question.
3. Example
For example, let’s say we’re looking at the question, 'Have you ever experienced discrimination or disrespectful/inappropriate behavior in your major department?' and the gender variable is selected.
In the top graph, when looking at the values for the 'No' response, female has a value of 110. This means 110 self-identified female students responded No. In the bottom graph, when looking at the values for the 'No' response, female has a value of 64%. This means 64% of all self-identified female students responded No.

Please return to the Welcome tab and scroll down to the, 'How do I use Build-a-Graph?' section if you'd like to see this example with the actual graph.

", size = "l")

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
      src = "techcollectivelogo.png",
      contentType = "image/png",
      height = "50px"
    )
  }, deleteFile = FALSE)

  output$photo1 <- renderImage({
    list(
      src = "example.png",
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
        if (str_detect(input$question, "experienced")){
          graphTitle = paste("Survey Prompt:", "Have you ever experienced discrimination or disrespectful/inappropriate\nbehavior in your major department?")
        }else{
          graphTitle = paste("Survey Prompt:", "Have you ever witnessed discrimination or disrespectful/inappropriate\nbehavior in your major department?")
        }

      }else if (input$qtype == "Course Satisfaction" & input$dep == "Department"){
        cdf = course_ldf
        graphTitle = paste("This data represents the overall course satisfaction for all",input$question, "courses.")
      }
      select_df <-
        cdf %>%
        filter(str_detect(question_text, str_escape(input$question)))
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
  ) %>% bindEvent(input$question,input$variable)

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

