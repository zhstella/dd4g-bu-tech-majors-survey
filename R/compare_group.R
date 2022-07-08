


compare_groups <- function(course_df, race_df, r1, r2) {
  # library(ordinal)
  # browser()
  rdf <- compare_group_df(course_df, race_df, r1, r2)

  # if(nrow(rdf) < 3) return(NA)
  z_value <- tryCatch(
    rdf %>%
      ordinal::clm(response ~ group, data = .) %>%
      broom::tidy() %>%
      filter(term == "group2") %>%
      pull(statistic),
    error = function(e) NA,
    warning = function(e) NA
  )

  rdf %>%
    group_by(group) %>%
    summarize(
      `# group` = n(),
      `average response` = round(mean(as.numeric(response)), 1)
    ) %>%
    complete(group) %>%
    pivot_wider(
      names_from = group,
      values_from = c(`# group`, `average response`),
      names_sep = " "
    ) %>%
    mutate(`z value` = z_value)
}


compare_group_df <- function(course_df, race_df, r1, r2) {
  race_str_list <- as.list(race_df$race_str)
  names(race_str_list) <- race_df$name
  bind_rows(
    course_df %>%
      filter_races(r1) %>%
      mutate(
        group = "1",
        str = str_c(race_str_list[r1], collapse = "\n ")
      ),
    course_df %>%
      filter_races(r2) %>%
      mutate(
        group = "2",
        str = str_c(race_str_list[r2], collapse = "\n ")
      )
  ) %>%
    mutate(group = factor(group, levels = c("1", "2")))
}
