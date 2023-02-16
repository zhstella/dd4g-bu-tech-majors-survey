simple_race_table <- function(ddf, race_df) {
  ddf %>% simplify_race_var(race_df) %>%
    group_by(race, .add = TRUE) %>%
    tally()
}


simplify_race_var <- function(ddf, race_df){
  ddf %>%
    mutate(
      race_full = race,
      race = ifelse(is.na(race), "NA", race),
      race = ifelse(str_detect(race, ","), "2 or more", race),
      race = race %>% fct_lump_min(20) %>% fct_infreq()
    )
}

# Cleaning functions

add_race_vars <- function(ddf, race_df) {
  if(FALSE){
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
  }
  ddf %>%
    select(ResponseId, race) %>%
    full_join(race_df, by = character()) %>%
    mutate(
      is_race = str_detect(race, fixed(race_str))
    ) %>%
    select(ResponseId, name, is_race) %>%
    pivot_wider(values_from = is_race) %>%
    full_join(ddf %>% select(-race), by = "ResponseId")
}


filter_races <- function(df, race_name) {
  df %>% filter(if_any(all_of(race_name), ~.))
}
