library(ggplot2)
library(scales)

stack_freq_prop <- function(g, title) {
  g1 <- g +
    geom_col(
      aes(y = count),
      position = position_dodge2(preserve = "single")
    ) +
    scale_x_discrete("", guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_fill_discrete(guide = "none", drop = FALSE) +
    geom_label(aes(y = count, label = as.character(count)), vjust = "inward", colour = "black", fontface = "bold", fill = "white", position = position_dodge(width = .9)) +
    ggtitle(title, subtitle = "Top: absolute counts. Bottom: relative proportions.") +
    theme(plot.title = element_text(size = 20, face = "bold"))
  g2 <- g +
    geom_col(
      aes(y = prop),
      position = position_dodge(preserve = "single")
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete("", drop = FALSE) +
    geom_label(aes(y = round(prop, digits = 2), label = as.character(percent(round(prop, digits = 2)))), vjust = "inward", colour = "black", fill = "white", fontface = "bold", position = position_dodge(width = .9)) +
    theme(legend.position = "bottom")
  ggpubr::ggarrange(g1, g2, nrow = 2, heights = c(1, 2), align = "v")
}


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
