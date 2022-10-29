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
  )
usethis::use_data(ddf, overwrite = TRUE)






satisfaction_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )

usethis::use_data(
  satisfaction_level,
  internal = TRUE,
  overwrite = TRUE
)
