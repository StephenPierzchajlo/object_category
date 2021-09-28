ratings <- function(rating_type){
  ggplot(ratings_data[ratings_data$rating_type == rating_type, ],
         aes(x = rating, fill = cue)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  facet_wrap(cue ~ modality, nrow = 4) +
  xlab("Rating") +
  ggtitle(rating_type)
  }