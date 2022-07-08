#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
##

# [1] "add_race_vars"  "course_df"      "course_num_re"  "ddf"            "filter_races"   "fn"
# [7] "gender_list"    "name_df"        "q_to_course_df" "race_df"        "response_level"



app <- function(...) {
  add_vars_to_env(ddf, env = environment())

  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
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
      ),
      tabPanel(
        "Course satisfaction correlation",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "course1",
              label = "Select first course",
              choices = unique(course_df$course)
            ),
            selectInput(
              "course2",
              label = "Select second course",
              choices = unique(course_df$course)
            )
          ),
          mainPanel(
            titlePanel("Satisfaction levels for pairs of courses"),
            # wellPanel(),
            htmlOutput("pair_explanation"),
            plotOutput("two_plot")
          )
        )
      ),
      compare_groups_ui("compare_groups")
    )
  )


  # Define server logic required to draw a histogram
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
    output$two_plot <- renderPlot({
      course1 <- as.name(input$course1)
      course2 <- as.name(input$course2)
      all_count <-
        course_df %>%
        filter(!is.na(response), course %in% c(input$course1, input$course2)) %>%
        pivot_wider(names_from = "course", values_from = "response") %>%
        count({{ course1 }}, {{ course2 }}, .drop = FALSE)
      g12 <- ggplot(
        all_count %>% filter(if_all(.fns = list(~ !is.na(.)))),
        aes(x = {{ course1 }}, y = {{ course2 }}, fill = n, label = n)
      ) +
        geom_tile() +
        geom_text(color = "red") +
        scale_x_discrete(drop = FALSE) +
        scale_y_discrete(drop = FALSE) +
        guides(x = "none", y = "none") +
        scale_fill_viridis_c()
      g1 <- all_count %>%
        filter(!is.na({{ course1 }})) %>%
        count({{ course1 }}, wt = n, name = "count", .drop = FALSE) %>%
        ggplot(aes(x = {{ course1 }}, y = count)) +
        geom_col() +
        scale_x_discrete(drop = FALSE) +
        labs(x = "", y = "")
      g2 <- all_count %>%
        filter(!is.na({{ course2 }})) %>%
        count({{ course2 }}, wt = n, name = "count", .drop = FALSE) %>%
        ggplot(aes(y = {{ course2 }}, x = count)) +
        geom_col() +
        scale_y_discrete(drop = FALSE) +
        labs(x = "", y = "")
      ggpubr::ggarrange(g2, g12, NULL, g1, ncol = 2, nrow = 2, heights = c(3, 1), widths = c(1, 3), common.legend = TRUE)
    })
    output$pair_explanation <- renderText({
      "The bottom and left plots show the total counts for each individual course. <br> The middle shows the counts for students who responded for both courses."
    })
    compare_group_server("compare_groups", course_df)
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}
