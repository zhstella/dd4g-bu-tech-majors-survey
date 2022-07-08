# Cleaning functions

add_race_vars <- function(ddf, race_df) {
  race_df <- ddf %>%
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
