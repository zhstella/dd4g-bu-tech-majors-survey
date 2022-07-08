compare_groups_ui <- function(id) {
  tabPanel(
    "Compare groups",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          NS(id, "group1"),
          label = "Select first group",
          choices = race_choice_list
        ),
        checkboxGroupInput(
          NS(id, "group2"),
          label = "Select second group",
          choices = race_choice_list
        )
      ),
      mainPanel(
        plotOutput(NS(id, "compare_plot")),
        wellPanel(htmlOutput(NS(id, "table_explanation"))),
        DTOutput(NS(id, "table"))
      )
    )
  )
}

compare_group_server <- function(id, course_df) {
  moduleServer(id, function(input, output, session) {
    course_compare <- reactive({
      g1 <- input$group1
      g2 <- input$group2
      course_df %>%
        group_by(course) %>%
        nest() %>%
        ungroup() %>%
        transmute(
          course,
          comp = map_dfr(data, ~ compare_groups(., race_df, g1, g2))
        ) %>%
        unnest(comp)
    })


    output$table_explanation <- renderText({
      c(
        "Negative z-values indicate that group 1 generally responded with higher",
        "levels of satisfaction and positive values indicate that group 2",
        "generally responded with higher levels of satisfaction. The larger",
        "the value, either positive or negative, the larger the gap. If no",
        "z-value is present, then there was not enough data to compare the",
        "groups for that course."
      )
    })
    output$table <- renderDT(
      {
        if (length(input$group1) > 0 && length(input$group2) > 0) {
          course_compare()
        } else {
          tibble(Note = "Please select at least one item for each group on the left.")
        }
      },
      selection = "single",
      rownames = FALSE
    )
    output$compare_plot <- renderPlot({
      r1 <- input$group1
      r2 <- input$group2
      if (!is.null(input$table_rows_selected)) {
        course_selected <- course_compare()$course[input$table_rows_selected]
        g <- course_df %>%
          compare_group_df(race_df, r1, r2) %>%
          filter(course %in% course_selected) %>%
          group_by(str, course) %>%
          count(response, name = "count", .drop = FALSE) %>%
          mutate(prop = count / sum(count)) %>%
          ggplot(aes(x = response, fill = str, group = str))
        title <- course_selected
      } else {
        g <- course_df %>%
          compare_group_df(race_df, r1, r2) %>%
          group_by(str) %>%
          count(response, name = "count", .drop = FALSE) %>%
          mutate(prop = count / sum(count)) %>%
          ggplot(aes(x = response, fill = str, group = str))
        title <- "All courses"
      }
      stack_freq_prop(g, title = title)
    })
  })
}
