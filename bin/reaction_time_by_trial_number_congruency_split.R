reaction_time_by_trial_number_congruency_split <- function(
  dataframe, label.y.1, label.y.2, title, y.lim.min, y.lim.max
  ) {
  ggplot(dataframe,
         aes(trial_number, reaction_time, group = congruency, fill = congruency, color = congruency)) +
    geom_point() +
    geom_smooth(method =lm) +
    theme_classic() +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") + 
    stat_cor(aes(color = congruency), label.x = c(0, 30), label.y = c(label.y.1, label.y.1), size = 3) +
    stat_regline_equation(aes(color = congruency),label.x = c(0,30), label.y = c(label.y.2, label.y.2),
                          size = 3) +
    xlab("Trial Number") +
    ylab("Average Reaction Time (ms)") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) +
    theme(text = element_text(size = 8)) +
    theme(legend.title = element_blank()) +
    ylim(y.lim.min, y.lim.max)
}