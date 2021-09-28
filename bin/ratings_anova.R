# Ratings ANOVA function.
rating_anova <- function(modality, rating_type) {
  anova <- aov_ez(
    "participant", "rating",
    ratings_data[ratings_data$modality == modality
                 & ratings_data$rating_type == rating_type, ], within = "cue"
    )
  kbl(nice(anova), caption = "anova", digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  }

# Ratings descriptives function.
rating_descriptives <- function(modality, rating_type){
  anova <- aov_ez(
    "participant", "rating",
    ratings_data[ratings_data$modality == modality 
                 & ratings_data$rating_type == rating_type, ], within = "cue")
  anova_d <- emmeans(anova, ~ cue)
  kbl(anova_d, caption = "descriptive statistics", digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  }

# Ratings pairwise comparison function.
rating_posthoc <- function(modality, rating_type){
  anova <- aov_ez(
    "participant", "rating",
    ratings_data[ratings_data$modality == modality
                 & ratings_data$rating_type == rating_type, ], within = "cue"
    )
  anova_d <- emmeans(anova, ~ cue)
  kbl(pairs(anova_d), caption = "post-hoc analysis", digits = 3) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
  }