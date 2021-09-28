accuracy_by_trial_number <- function(
  dataframe, label.x.1, label.y.1, label.x.2, label.y.2, title, y.lim.min, y.lim.max
  ) {
  ggplot(dataframe, aes(trial_number, accuracy)) + geom_point() +
    geom_smooth(method =lm) +
    theme_classic() +
    stat_cor(label.x = label.x.1, label.y = label.y.1) +
    stat_regline_equation(label.x = label.x.2, label.y = label.y.2) +
    xlab("Trial Number") +
    ylab("Average Accuracy") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.title = element_text(size = 8)) +
    theme(text = element_text(size = 8)) +
    theme(legend.title = element_blank()) +
    ylim(y.lim.min, y.lim.max)
}