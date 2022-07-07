#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
##

# [1] "add_race_vars"  "course_df"      "course_num_re"  "ddf"            "filter_races"   "fn"
# [7] "gender_list"    "name_df"        "q_to_course_df" "race_df"        "response_level"



app <- function(...) {

  race_choice_list <- as.list(race_df$name)
  names(race_choice_list) <- race_df$race_str
  race_str_list <- as.list(race_df$race_str)
  names(race_str_list) <- race_df$name

# Define UI for application that draws a histogram
  ui <- fluidPage(

      # Application title
    titlePanel("DEI Student Survey"),
    tabsetPanel(
      tabPanel(
        "Course satisfication",
        # Sidebar with a slider input for number of bins
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
        # Sidebar with a slider input for number of bins
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
      tabPanel(
        "Compare groups",
        sidebarLayout(
          sidebarPanel(
            checkboxGroupInput(
              "group1",
              label = "Select first group",
              choices = race_choice_list
            ),
            checkboxGroupInput(
              "group2",
              label = "Select second group",
              choices = race_choice_list
            )
          ),
          mainPanel(
            plotOutput("compare_plot"),
            wellPanel(htmlOutput("table_explanation")),
            DTOutput("table")
          )
        )
      )

    )
  )


  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$freq_plot <- renderPlot({
      if(length(input$course) > 0) {
        g <- course_df %>%
          filter(course %in% input$course)  %>%
          group_by(course) %>%
          count(response, name = "count", .drop = FALSE)%>%
          mutate(prop = count / sum(count)) %>%
          ggplot(aes(x = response, fill = course, group = course))
        title <- "Responses for selected course(s)"
      } else {
        g <- course_df %>%
          count(response, name = "count", .drop = FALSE) %>%
          mutate(prop = count / sum(count)) %>%
          ggplot(aes(x = response, group = factor(1)))
        title = "All responses"
      }
      stack_freq_prop(g, title = title)
    }, height = 600)
    output$two_plot <- renderPlot({
      course1 <- as.name(input$course1)
      course2 <- as.name(input$course2)
      all_count <-
        course_df %>%
          filter(!is.na(response), course %in% c(input$course1, input$course2)) %>%
          pivot_wider(names_from = "course", values_from = "response") %>%
          count({{ course1 }}, {{ course2 }}, .drop = FALSE)
      g12 <- ggplot(
        all_count %>% filter(if_all(.fns = list(~!is.na(.)))),
        aes(x = {{ course1 }}, y = {{ course2 }}, fill = n, label = n)
      ) +
        geom_tile() +
        geom_text(color = "red") +
        scale_x_discrete(drop = FALSE) +
        scale_y_discrete(drop = FALSE) +
        guides(x = "none", y = "none") +
        scale_fill_viridis_c()
      g1 <- all_count %>% filter(!is.na({{course1}})) %>%
        count({{course1}}, wt = n, name = "count", .drop = FALSE) %>%
        ggplot(aes(x = {{course1}}, y  = count)) +
        geom_col()  +
        scale_x_discrete(drop = FALSE) +
        labs(x = "", y = "")
      g2 <- all_count %>% filter(!is.na({{course2}})) %>%
        count({{course2}}, wt = n, name = "count", .drop = FALSE) %>%
        ggplot(aes(y = {{course2}}, x = count)) +
        geom_col() +
        scale_y_discrete(drop = FALSE) +
        labs(x = "", y = "")
      ggpubr::ggarrange(g2, g12, NULL, g1, ncol = 2, nrow = 2, heights = c(3,1), widths = c(1, 3), common.legend = TRUE)
    })
    output$pair_explanation <- renderText({
      "The bottom and left plots show the total counts for each individual course. <br> The middle shows the counts for students who responded for both courses."
    })
    output$table_explanation <- renderText({
      "Negative z-values indicate that group 1 generally responded with higher levels of satisfaction and positive values indicate that group 2 generally ersponded with higher levels of satisfaction. The larger the value, either positive or negative, the larger the gap. If no z-value is present, then there was not enough data to compare the groups for that course."
    })
    course_compare <- reactive({
      g1 <- input$group1
      g2 <- input$group2
      course_df %>%
        group_by(course) %>%
        nest() %>%
        ungroup() %>%
        transmute(
          course,
          comp = map_dfr(data, ~compare_groups(., g1, g2))
        ) %>%
        unnest(comp)
    })
    output$table <- renderDT(
      {
        if(length(input$group1) > 0 && length(input$group2) > 0) {
          course_compare()
        } else{
          tibble(Note = "Please select at least one item for each group on the left.")
        }

      },
      selection = 'single',
      rownames = FALSE
    )
    output$compare_plot <- renderPlot(
      {
        r1 <- input$group1
        r2 <- input$group2
        if(!is.null(input$table_rows_selected)) {
          course_selected <- course_compare()$course[input$table_rows_selected]
          g <- course_df %>%
            compare_group_df(r1, r2) %>%
            filter(course %in% course_selected) %>%
            group_by(str, course) %>%
            count(response, name = "count", .drop = FALSE) %>%
            mutate(prop = count / sum(count)) %>%
            ggplot(aes(x = response, fill = str, group = str))
          title <- course_selected
        } else {
          g <- course_df %>%
            compare_group_df(r1, r2) %>%
            group_by(str) %>%
            count(response, name = "count", .drop = FALSE) %>%
            mutate(prop = count / sum(count)) %>%
            ggplot(aes(x = response, fill = str, group = str))
          title <- "All courses"
        }
        stack_freq_prop(g, title = title)
      }
    )
  }

  # Run the application
  shinyApp(ui = ui, server = server)
}

