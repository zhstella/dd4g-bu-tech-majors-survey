

library(tidyverse)

load("ddf.rda")
load("original_question_df.rda")




satisfaction_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )

# create gender variables
gender_df <- ddf %>% count(gender)

# Create race variables based off the data set
race_df <- ddf %>%
  count(race) %>%
  filter(!str_detect(race, ",")) %>% # assumes each race was picked alone at least once
  transmute(
    name = tolower(race) %>%
      str_extract("[\\w\\s]+") %>%
      str_remove_all("\\s*$") %>%
      str_replace_all("\\s", "_") %>%
      str_c("race_", .),
    race_str = race
  )

race_choice_list <- as.list(race_df$name)
names(race_choice_list) <- race_df$race_str

race_str_list <- as.list(race_df$race_str)
names(race_str_list) <- race_df$name

# Create course based data frame

# create tibble for joining to get course codes
q_to_course_df <-
  original_question_df %>%
  filter(str_detect(question_text, "How satis")) %>%
  mutate(course = str_remove(question_text, "^.*? - "))
# regular expression for courses
course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
course_df <- ddf %>%
  add_race_vars(race_df) %>%
  select(RecordedDate, ResponseId, starts_with("race"), starts_with("Q")) %>%
  pivot_longer(
    starts_with("Q"),
    names_to = "question_id",
    values_to = "response"
  ) %>%
  right_join(q_to_course_df, by = "question_id") %>%
  select(RecordedDate, ResponseId, starts_with("race"), course, response) %>%
  filter(!is.na(response), response != "Did not take this course.") %>%
  mutate(response = factor(response, levels = satisfaction_level)) %>%
  mutate(course = str_c(
    str_extract(course, course_num_re),
    str_remove(course, course_num_re)
  ))
