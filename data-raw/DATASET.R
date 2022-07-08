## code to prepare `DATASET` dataset goes here
fn <- here::here(
  "data-raw",
  "Current & Alum Surveys All Raw Data_June 9, 2022_14.22.xlsx"
)

ddf <- readxl::read_xlsx(fn, skip = 1)
name_df <- readxl::read_xlsx(fn, n_max = 1)
names(ddf) <- names(name_df)
original_question_df <- name_df %>%
  pivot_longer(everything(), names_to = "question_id", values_to = "question_text")
usethis::use_data(original_question_df, overwrite = TRUE)

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
usethis::use_data(ddf, overwrite = TRUE)




race_df <- ddf %>%
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
race_choice_list <- as.list(race_df$name)
names(race_choice_list) <- race_df$race_str
race_str_list <- as.list(race_df$race_str)
names(race_str_list) <- race_df$name

usethis::use_data(race_df, race_choice_list, race_str_list, internal = TRUE, overwrite = TRUE)
