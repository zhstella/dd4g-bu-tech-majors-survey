# Load and clean the data



add_race_vars <- function(ddf, race_df) {
  race_df <- ddf %>% select(ResponseId, race) %>%
    full_join(race_df, by = character()) %>%
    mutate(
      is_race = str_detect(race, fixed(race_str))
    ) %>%
    select(ResponseId, name, is_race) %>%
    pivot_wider(values_from = is_race) %>%
    full_join(ddf %>% select(-race), by = "ResponseId")
}


filter_races <- function(df, race_name){
  df %>% filter(if_any(all_of(race_name), ~.))
}

add_vars_to_env <- function(ddf, env = environment()) {
env$q_to_course_df <-
  original_question_df %>%
  filter(str_detect(question_text, "How satis")) %>%
  mutate(course =  str_remove(question_text, "^.*? - "))

env$response_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )

env$race_df <- ddf %>%
  count(race) %>%
  filter(!str_detect(race, ",")) %>%
  transmute(
    name = tolower(race) %>%
      str_extract("[\\w\\s]+") %>%
      str_remove_all("\\s*$") %>%
      str_replace_all("\\s", "_") %>%
      str_c("race_", .),
    race_str = race
  )



env$gender_list <- ddf %>% count(gender)


course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
env$course_df <- ddf %>%
  add_race_vars(env$race_df) %>%
  select(RecordedDate, ResponseId, starts_with("race"), starts_with("Q")) %>%
  pivot_longer(
    starts_with("Q"),
    names_to = "question_id",
    values_to = "response"
  ) %>%
  right_join(env$q_to_course_df, by = "question_id") %>%
  select(RecordedDate, ResponseId, starts_with("race"), course, response) %>%
  filter(!is.na(response), response != "Did not take this course.") %>%
  mutate(response = factor(response, levels = env$response_level)) %>%
  mutate(course = str_c(
    str_extract(course, course_num_re),
    str_remove(course, course_num_re)
  ))


}
