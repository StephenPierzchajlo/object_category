rating_anova <- function(modality, rating_type) {
  
  #'@title ANOVA for ratings data.
  #'@author Stephen Pierzchajlo
  #'@description Outputs ANOVA for ratings of specific modality and stimulus.
  #'@usage rating_anova(modality, rating_type).
  #'@param modality either visual or olfactory.
  #'@param rating_type either intensity, quality, floweriness, or fruitness.
  #'@details Function performs anova for specific modality and rating type, and then outputs the result
  #'in a nice table via the kable_extra package.
  
  anova <- aov_ez(
    "participant", "rating",
    ratings_data[ratings_data$modality == modality & ratings_data$rating_type == rating_type, ],
    within = "cue"
    )
  kbl(nice(anova), caption = "anova", digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  }

rating_descriptives <- function(modality, rating_type){
  
  #'@title Descriptive statistics for ratings data.
  #'@author Stephen Pierzchajlo
  #'@description Table for descriptive ratings data.
  #'@usage rating_descriptives(modality, rating_type).
  #'@param modality either visual or olfactory.
  #'@param rating_type either intensity, quality, floweriness, or fruitness.
  #'@details Function outputs a table of averag ratings values for a particular rating type and
  #'particular sensory modality.
  
  anova <- aov_ez(
    "participant", "rating",
    ratings_data[ratings_data$modality == modality & ratings_data$rating_type == rating_type, ],
    within = "cue"
    )
  anova_d <- emmeans(anova, ~ cue)
  kbl(anova_d, caption = "descriptive statistics", digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  }

rating_posthoc <- function(modality, rating_type){
  
  #'@title Post-hoc statistics for ratings data.
  #'@author Stephen Pierzchajlo
  #'@description Outputs post-hoc analysis for ratings of specific modality and stimulus.
  #'@usage rating_posthoc(modality, rating_type).
  #'@param modality either visual or olfactory.
  #'@param rating_type either intensity, quality, floweriness, or fruitness.
  #'@details Function performs post-hoc test for specific modality and rating type, and then outputs the result
  #'in a nice table via the kable_extra package.
  
  anova <- aov_ez(
    "participant", "rating",
    ratings_data[ratings_data$modality == modality & ratings_data$rating_type == rating_type, ],
    within = "cue"
    )
  anova_d <- emmeans(anova, ~ cue)
  kbl(pairs(anova_d), caption = "post-hoc analysis", digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  }