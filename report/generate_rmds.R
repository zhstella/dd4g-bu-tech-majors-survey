
# Load in course questions

devtools::load_all()
course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
course_satisfaction_q <- original_question_df %>%
  filter(
    str_detect(question_text, "satisfied"),
    str_detect(question_text, "agreement", negate = TRUE)
  ) %>%
  mutate(question_text = str_extract(question_text, course_num_re))

# Write department satisfaction Rmd

department_satisfaction_template <-
  read_lines("report/templates/department_satisfaction_template.Rmd")

dep_tbl <-
  tibble(
    department =
      sort(unique(str_sub(course_satisfaction_q$question_text, 1, 6)))
  ) %>%
  mutate(dep_code = str_replace_all(department, "\\s*", ""))

dep_tbl %>%
  pmap_chr(function(department, dep_code){
    whisker::whisker.render(
      department_satisfaction_template
    )
  }) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Department Satisfaction\n\n", .) %>%
  write_lines("report/01-Department-Satisfaction.Rmd")

# Write course satisfaction Rmd
course_satisfaction_template <-
  read_lines("report/templates/course_satisfaction_template.Rmd")

course_tbl <-
  tibble(
    course =
      sort(unique(course_satisfaction_q$question_text))
  ) %>%
  mutate(course_code = str_replace_all(course, "\\s*", "")) %>%
  distinct(course_code, .keep_all = TRUE)

course_tbl %>%
  pmap_chr(function(course, course_code){
    whisker::whisker.render(
      course_satisfaction_template
    )
  }) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Course Satisfaction\n\n", .) %>%
  write_lines("report/02-Course-Satisfaction.Rmd")


# Get "agreement" questions

agreement_level <- c(
  "Strongly disagree",
  "Never",
  "Disagree",
  "Rarely",
  "Agree",
  "Sometimes",
  "Strongly agree",
  "Often times",
  "Prefer not to say"
)

agreement_q <- original_question_df %>%
  filter(
    str_detect(question_text, "agreement")
  ) %>%
  mutate(question_text = str_extract(question_text, "(?<=: - ).*"))

agree_q_tbl <-
  tibble(
    agree_q =
      sort(unique(agreement_q$question_text))
  ) %>%
  mutate(agree_q_code = str_replace_all(agree_q, "\\W+", "_")) %>%
  distinct(agree_q_code, .keep_all = TRUE)

agree_q_tbl %>%
  pmap_chr(function(agree_q, agree_q_code){
    whisker::whisker.render(
      read_lines("report/templates/agreement_template.Rmd")
    )
  }) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Agreement Statements\n\n\n", .) %>%
  write_lines("report/03-Agreement.Rmd")

# Adjectives




adjectives_q <- original_question_df %>%
  filter(
    str_detect(question_text, "adjectives that best represents")
  ) %>%
  mutate(question_text = str_extract(question_text, "(?<=\\. - ).*"))

adjectives_ldf <- ddf %>%
  pivot_longer(
    cols = adjectives_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(adjectives_q, by = "question_id") %>%
  mutate(response = factor(response, levels = c("1", "2", "3", "4", "5")))

adjectives_q_tbl <-
  tibble(
    adjectives_q =
      sort(unique(adjectives_q$question_text))
  ) %>%
  mutate(adjectives_q_code = str_replace_all(adjectives_q, "\\W+", "_")) %>%
  distinct(adjectives_q_code, .keep_all = TRUE)

adjectives_q_tbl %>%
  pmap_chr(function(adjectives_q, adjectives_q_code){
    whisker::whisker.render(
      read_lines("report/templates/adjectives_template.Rmd")
    )
  }) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Adjective Range Questions\n\n\n", .) %>%
  write_lines("report/04-Agreement.Rmd")

# Serve Book
bookdown::render_book("report/")
bookdown::serve_book("report/")

