accuracy_by_reaction_time <- function(dataframe, stat_cor.x, stat_cor.y, stat_reg.x, stat_reg.y, title) {
  ggplot(dataframe, aes(x = accuracy, y = diff)) + 
    geom_point() +
    geom_smooth(method = lm) +
    theme_classic() +
    stat_cor(label.x = stat_cor.x, label.y = stat_cor.y) +
    stat_regline_equation(label.x = stat_reg.x, label.y = stat_reg.y) +
    xlab("Average Accuracy") +
    ylab("Congruency Reaction Time Difference (ms)") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=12)) +
    theme(axis.title.y = element_text(size = 7))
  }