reaction_time_by_trial_number <- function(
  dataframe, label.y.1, label.y.2,  title, y.lim.min, y.lim.max
  ) {
  ggplot(dataframe, aes(trial_number, reaction_time)) + geom_point() +
    geom_smooth(method = lm) +
    theme_classic() +
    stat_cor(label.x = 0, label.y = label.y.1, size = 4) +
    stat_regline_equation(label.x = 0, label.y = label.y.2, size = 4) +
    xlab("Trial Number") +
    ylab("Average Reaction Time (ms)") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size=8)) +
    theme(text = element_text(size=8)) +
    theme(legend.title = element_blank()) +
    ylim(y.lim.min, y.lim.max)
}