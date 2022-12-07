stack_freq_prop <- function(g, title = "Frequency chart") {
  g1 <- g +
    geom_col(aes(y = count), position = position_dodge(preserve = "single")) +
    scale_x_discrete("", guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_fill_discrete(guide = "none", drop = FALSE) +
    ggtitle(title, subtitle = "Top: absolute counts. Bottom: relative proportions.")
  g2 <- g +
    geom_col(
      aes(y = prop),
      position = position_dodge(preserve = "single")
    ) +
    scale_x_discrete(guide = guide_none(), drop = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete("", drop = FALSE, position = "bottom") +
    theme(legend.position = "bottom")
  ggpubr::ggarrange(g1, g2, nrow = 2, heights = c(1, 2))
}
