# Plot congruency effects.
effect_of_congruency <- function(dataframe, title, ylower, yupper, geomtext_x, geomtext_y){
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