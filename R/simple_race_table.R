
#' Title
#'
#' @param ddf
#' @param race_df
#'
#' @return
#' @export
#'
#' @examples
simple_race_table <- function(ddf, race_df) {
  ddf %>% simplify_race_var(race_df) %>%
    group_by(race, .add = TRUE) %>%
    tally()
}


simplify_race_var <- function(ddf, race_df){
  ddf %>%
    mutate(
      race = ifelse(is.na(race), "NA", race),
      race = ifelse(str_detect(race, ","), "2 or more", race),
      race = race %>% fct_lump_min(20) %>% fct_infreq()
    )
}
