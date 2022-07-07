# Load and clean the data

load_data <- function(){
  fn <- here::here("Current & Alum Surveys All Raw Data_June 9, 2022_14.22.xlsx")
  ddf <- readxl::read_xlsx(fn, skip = 1)
  name_df <- readxl::read_xlsx(fn, n_max = 1)
  names(ddf) <- names(name_df)

  ddf <- ddf %>%
    rename(
      major = Q1,
      major_other_txt = Q1_7_TEXT,
      grad_year = Q2,
      minor = Q3,
      minor_other_txt = Q3_6_TEXT,
      first_gen = Q4,
      international = Q5,
      international_other_txt = Q5_3_TEXT,
      gender = Q6,
      gender_other = Q6_4_TEXT,
      race = Q7,
      race_other_txt = Q7_8_TEXT,
      pronouns = Q8,
      pronouns_other_txt = Q8_15_TEXT,
      options = Q9,
      prepared = Q10
    ) %>%
    rename_with(
      ~str_c("adj_", str_sub(., 5)),
      .cols = starts_with("Q24")
    )
}

aa <- function() {
name_df %>%
  pivot_longer(everything()) %>%
  mutate(v = str_sub(value, 1, 25)) %>%
  count(v) %>%
  arrange(-n)

q_to_course_df <- name_df %>%
  pivot_longer(everything()) %>%
  filter(str_detect(value, "How satis")) %>%
  mutate(course =  str_remove(value, "^.*? - "))

response_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )

race_df <-
  ddf %>%
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



gender_list <- ddf %>% count(gender)

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

course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
course_df <- ddf %>%
  add_race_vars(race_df) %>%
  select(RecordedDate, ResponseId, starts_with("race"), starts_with("Q")) %>%
  pivot_longer(starts_with("Q"), values_to = "response") %>%
  right_join(q_to_course_df) %>%
  select(RecordedDate, ResponseId, starts_with("race"), course, response) %>%
  filter(!is.na(response), response != "Did not take this course.") %>%
  mutate(response = factor(response, levels = response_level)) %>%
  mutate(course = str_c(
    str_extract(course, course_num_re),
    str_remove(course, course_num_re)
  ))


filter_races <- function(df, race_name){
  df %>% filter(if_any(all_of(race_name), ~.))
}
}
