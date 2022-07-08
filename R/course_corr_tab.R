
course_corr_ui <- function(id) {
  tabPanel(
    "Course satisfaction correlation",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          NS(id, "course1"),
          label = "Select first course",
          choices = unique(course_df$course)
        ),
        selectInput(
          NS(id, "course2"),
          label = "Select second course",
          choices = unique(course_df$course)
        )
      ),
      mainPanel(
        titlePanel("Satisfaction levels for pairs of courses"),
        # wellPanel(),
        htmlOutput(NS(id, "pair_explanation")),
        plotOutput(NS(id, "two_plot"))
      )
    )
  )
}

course_corr_server <- function(id) {
  moduleServer(id, function(input, output, session) {
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
      c(
        "The bottom and left plots show the total counts for each individual course.",
        "<br> The middle shows the counts for students who responded for both courses."
      )
    })
  })
}
