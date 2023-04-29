## code to prepare `DATASET` dataset goes here
fn <- here::here(
  "data-raw",
  "Computing and Data Sciences Climate Survey_April 21, 2023_08.24.csv"
  # "Current & Alum Surveys All Raw Data_June 9, 2022_14.22.xlsx"
)

fn_old <- here::here(
  "data-raw",
  "Current & Alum Surveys All Raw Data_June 9, 2022_14.22.xlsx"
)

ddf_new <- read_csv(fn, skip=3, col_names = FALSE)
ddf <- readxl::read_xlsx(fn_old, skip=2, col_names = FALSE)
name_df <- readxl::read_xlsx(fn_old, n_max = 1)
names(ddf_new) <- names(name_df)
names(ddf) <- names(name_df)

ddf <- ddf %>%
  select(-Q19, -Q34) %>%
  left_join(ddf_new %>% select(StartDate, EndDate, IPAddress, Q19, Q34))
original_question_df <- name_df %>%
  pivot_longer(everything(), names_to = "question_id", values_to = "question_text")
save(original_question_df, file = "R/original_question_df.rda")

ddf <- ddf %>%
  mutate(work_status = Q21) %>%
  mutate(prep = Q10) %>%
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
    pronouns_other_txt = Q8_15_TEXT
  )
save(ddf, file = "R/ddf.rda")






satisfaction_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )
#
# usethis::use_data(
#   satisfaction_level,
#   internal = TRUE,
#   overwrite = TRUE
# )
