
# Load in course questions

devtools::load_all()

# set to -1 to make the whole book
max_q <- 3

# Load template

general_template <- read_lines("report/templates/general_template.Rmd")

whisker_template <- function(
    selected_q,
    selected_q_code,
    title,
    subsection_title,
    which_df,
    template = general_template
){
  whisker::whisker.render(template)
}

# Get course satisfactin questions
course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
course_satisfaction_q <- original_question_df %>%
  filter(
    str_detect(question_text, "satisfied"),
    str_detect(question_text, "agreement", negate = TRUE)
  ) %>%
  mutate(question_text = str_extract(question_text, course_num_re))

# Write department satisfaction Rmd
(dep_tbl <-
  tibble(
    selected_q =
      sort(unique(str_sub(course_satisfaction_q$question_text, 1, 6)))
  ) %>%
  mutate(
    selected_q_code = str_replace_all(selected_q, "\\W+", "_"),
    title = str_glue("{selected_q} Department Satisfaction"),
    subsection_title = "Satisfaction",
    which_df = "course"
  )) %>%
  head(max_q) %>%
  pmap_chr(whisker_template) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Department Satisfaction\n\n", .) %>%
  write_lines("report/01-Department-Satisfaction.Rmd")

# Write course satisfaction Rmd

(course_tbl <-
  tibble(
    selected_q =
      sort(unique(course_satisfaction_q$question_text))
  ) %>%
  mutate(
    selected_q_code = str_replace_all(selected_q, "\\W+", "_"),
    title = str_glue("{selected_q} Course Satisfaction"),
    subsection_title = "Satisfaction",
    which_df = "course"
  )) %>%
  head(max_q) %>%
  pmap_chr(whisker_template) %>%
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

(agree_q_tbl <-
  tibble(
    selected_q =
      sort(unique(agreement_q$question_text))
  ) %>%
  mutate(
    selected_q_code = str_replace_all(selected_q, "\\W+", "_"),
    title = str_glue("{selected_q} Agreement"),
    subsection_title = "Agreement",
    which_df = "agreement"
  )) %>%
  head(max_q) %>%
  pmap_chr(whisker_template) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Agreement Statements\n\n\n", .) %>%
  write_lines("report/03-Agreement.Rmd")

# Adjectives
adjectives_q <- original_question_df %>%
  filter(
    str_detect(question_text, "adjectives that best represents")
  ) %>%
  mutate(question_text = str_extract(question_text, "(?<=\\. - ).*"))

(adjectives_q_tbl <-
  tibble(
    selected_q =
      sort(unique(adjectives_q$question_text))
  ) %>%
  mutate(
    selected_q_code = str_replace_all(selected_q, "\\W+", "_"),
    title = str_glue("{selected_q} Adjective Range"),
    subsection_title = "Adjective Range",
    which_df = "adj"
  )) %>%
  head(max_q) %>%
  pmap_chr(whisker_template) %>%
  str_c(collapse = "\n\n") %>%
  str_c("# Adjective Range Questions\n\n\n", .) %>%
  write_lines("report/04-Adjectives.Rmd")

# Serve Book
bookdown::render_book("report/", output_format = "all")

