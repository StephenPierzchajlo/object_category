ratings <- function(rating_type) {
  #'@title Ratings graph.
  #'@author Stephen Pierzchajlo
  #'@description Plots histogram of specific rating type, split by modality.
  #'@usage ratings(rating_type)
  #'@param rating_type Can take 1 of 4 arguements: intensity, quality, fruitiness, or floweriness.
  #'@details This function takes the ratings_data dataframe and conditions on the rating type the
  #'user inputs into the function. A histogram is plotted, and wrapped based on modality. It should 
  #'produce a 4x4 graph with ratings for each stimulus and each modality for the specified rating
  #'type.
  ggplot(ratings_data[ratings_data$rating_type == rating_type, ],
         aes(x = rating, fill = cue)) +
  geom_histogram(position = "identity", alpha = 0.5) +
  facet_wrap(cue ~ modality, nrow = 4) +
  xlab("Rating") +
  ggtitle(rating_type)
  }