reaction_time_by_trial_number_congruency_split <- function(
  dataframe, label.y.1, label.y.2, title, y.lim.min, y.lim.max
  ) {
  
  #'@title Plot reaction time by trial number: Split by congruency.
  #'@author Stephen Pierzchajlo
  #'@description Plots correlation between reaction time and trial number, split by congruency.
  #'@usage reaction_time_by_trial_number_congruency_split(
  #'dataframe, label.y.1, label.y.2, title, y.lim.min, y.lim.max
  #')
  #'@param dataframe The data to be plotted.
  #'@param label.y.1 The lowest y-value to be included.
  #'@param label.y.2 The largest y-value to be included.
  #'@param title Title of graph. In quotations.
  #'@param y.lim.min Adjusts lower linit of data spread.
  #'@param y.lim.max Adjusts upper limit of data spread.
  #'@details This function returns a graph with regression lines and data for the correlation between
  #'congruent and incongruent trials. The data and regression lines are further split and colored by
  #'whether they come from congruent or incongruent trials.

  ggplot(dataframe,
         aes(trial_number, reaction_time, group = congruency, fill = congruency, color = congruency)) +
    geom_point() +
    geom_smooth(method =lm) +
    theme_classic() +
    scale_fill_brewer(palette = "Set1") +
    scale_color_brewer(palette = "Set1") + 
    stat_cor(aes(color = congruency), label.x = c(0, 30), label.y = c(label.y.1, label.y.1), size = 3) +
    stat_regline_equation(
      aes(color = congruency),label.x = c(0,30), label.y = c(label.y.2, label.y.2), size = 3
      ) +
    xlab("Trial Number") +
    ylab("Average Reaction Time (ms)") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = 8)) +
    theme(text = element_text(size = 8)) +
    theme(legend.title = element_blank()) +
    ylim(y.lim.min, y.lim.max)
  }