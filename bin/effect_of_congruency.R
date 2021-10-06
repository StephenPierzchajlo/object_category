effect_of_congruency <- function(dataframe, title, ylower, yupper, geomtext_x, geomtext_y) {
  #'@title Congruency difference graph.
  #'@author Stephen Pierzchajlo
  #'@description Plots congruency difference between modalities with lines connecting individuals.
  #'@usage effect_of_congruency(dataframe, title, ylower, yupper, geomtext_x, geomtext_y)
  #'@param dataframe Dataframe generated from the coloring() function.
  #'@param title Title to go above graph.
  #'@param ylower Lowest y-axis value to be included in graph.
  #'@param yupper Highest y-axis value to be included in graph.
  #'@param geomtext_x Position of on-graph text along x-axis.
  #'@param geomtext_y Position of on'graph text along y-axis.
  #'@details This function takes a dataframe made by the coloring() function, and outputs a graph.
  #'The graph contains difference between congruent and incongruent reaction times for both
  #' object-cue and category-cue blocks, with lines connecting congruent and incongruent reaction
  #' times for each participant. The shading of the lines denotes whether difference in performance
  #' is faster or slower per participant. 
  ggplot(dataframe, aes(y = reaction_time, shape = congruency)) +
    geom_violin(aes(x = congruency, group = congruency, color = congruency,
                    fill = congruency, width = .5)) +
    scale_fill_brewer(palette = "BuPu") +
    scale_color_brewer(palette = "BuPu") +
    geom_line(aes(group = participant, x = congruency,  color = coloring,
                  linetype = coloring)) +
    scale_fill_brewer(palette = "BuPu") +
    scale_color_brewer(palette = "BuPu") +
    scale_linetype_manual(values = c("solid", "dotted")) +
    theme_classic() +
    facet_wrap(~cue_type) +
    geom_text(aes(x = geomtext_x, y = geomtext_y, label = paste0(annotation, "%"))) +
    scale_shape(guide = FALSE) +
    ggtitle(title) +
    xlab("") +
    ylab("Reaction Time (ms)") +
    ylim(ylower, yupper) +
    theme(panel.spacing = unit(2, "lines"),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 10),
          legend.position = "none",
          text = element_text(size = 8))
  }