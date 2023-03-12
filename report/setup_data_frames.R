
library(tidyverse)
library(gt)
source(here::here("R/source_all.R"))

ddf_s <- ddf %>%
  mutate(
    gender = as_factor(ifelse(
      str_detect(gender, "not listed"),
      "Pref. not listed",
      gender
    )),
    international = as_factor(replace_na(international, "No response")),
    first_gen = as_factor(replace_na(first_gen, "No response"))
  ) %>%
  simplify_race_var()

course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
course_satisfaction_q <- original_question_df %>%
  filter(
    str_detect(question_text, "satisfied"),
    str_detect(question_text, "agreement", negate = TRUE)
  ) %>%
  mutate(question_text = str_extract(question_text, course_num_re))
satisfaction_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )

course_ldf <- ddf_s %>%
  pivot_longer(
    cols = course_satisfaction_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  filter(response != "Did not take this course.") %>%
  left_join(course_satisfaction_q, by = "question_id") %>%
  mutate(
    response =
      factor(
        response,
        levels = satisfaction_level,
        ordered = TRUE
      )
  )

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

frequency_level <- c(
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
  mutate(question_text = str_c(" ", str_extract(question_text, "(?<=: - ).*")))

agreement_ldf <- ddf_s %>%
  pivot_longer(
    cols = agreement_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(agreement_q, by = "question_id") %>%
  mutate(response = factor(response, levels = agreement_level))

# Adjectives

adjectives_q <- original_question_df %>%
  filter(
    str_detect(question_text, "adjectives that best represents")
  ) %>%
  mutate(question_text = str_extract(question_text, "(?<=\\. - ).*"))

adj_ldf <- ddf_s %>%
  pivot_longer(
    cols = adjectives_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(adjectives_q, by = "question_id") %>%
  mutate(response = factor(response, levels = c("1", "2", "3", "4", "5")))

dis_q <- original_question_df %>%
  filter(question_id == "Q36" | question_id == "Q38")

dis_ldf <- ddf_s %>%
  pivot_longer(
    cols = dis_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(dis_q, by = "question_id")
