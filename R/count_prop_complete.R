#' @export
count_prop_complete <- function(df, ..., .fill = TRUE) {
  fill_vals <- list(prop = NA, count =  NA)
  if(.fill){
    fill_vals <- list(prop = 0, count = 0)
  }

  agreement_level <- c(
    "Strongly disagree",
    "Disagree",
    "Agree",
    "Strongly agree"
  )

  frequency_level <- c(
    "Never",
    "Rarely",
    "Sometimes",
    "Often times"
  )


  if(any(as.character(df$response) %in% agreement_level)){
    df <- df %>% mutate(response = fct_drop(response, only = frequency_level))
   } else if(any(as.character(df$response) %in% frequency_level)){
    df <- df %>% mutate(response = fct_drop(response, only = agreement_level))
   }

  df %>%
    group_by(...) %>%
    count(response, name = "count") %>%
    mutate(prop = count / sum(count)) %>%
    ungroup() %>%
    complete(..., response, fill = fill_vals) %>%
    group_by(...)
}
