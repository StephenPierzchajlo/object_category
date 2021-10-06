estimate_mode <- function(x) {
  #'@title Estimate Mode
  #'@author Stephen Pierzchajlo
  #'@description Estimates mode of distribution
  #'@usage estimate_mode(x)
  #'@param x vector of data.
  #'@details Function models the density of inputted vector, outputting a single value: the mode of
  #'the distribution.
  d <- density(x)
  d$x[which.max(d$y)]
}

draws <- function(n_success) {
  #'@title Draws
  #'@author Stephen Pierzchajlo
  #'@description Draws values from a binomial distribution with n successes and calculates 95%
  #'credibility interval.
  #'@usage draws()
  #'@param n_success Number of successes in n draws.
  #'@details 
  
  # Number of draws
  n_draws <- 5e5
  
  # Data (proportion of successful participants, because a posterior is easy to compute).
  n_participants <- 48
  n_success <- n_success
  success_rate <- n_success / n_participants
  
  # Non-informed (uniform distribution).
  prior_unif <- runif(n_draws, min = 0, max = 1)
  
  # Generates n_draws of data withn_participants.
  a <- rbinom(n = n_draws, size = n_participants, prob = prior_unif)
  
  # Select draws where n_success was generated.
  posterior_unif <- prior_unif[a == n_success]
  
  # Should be at least 5000 (make n_draws larger if not).
  length(posterior_unif)
  
  # The mode is the "Maximum likelihood estimate".
  (maxL_unif <- estimate_mode(posterior_unif))
  
  # 95% credible interval.
  ci95_unif <- quantile(posterior_unif, c(0.025, 0.975))
  
  # Posterior stored in dataframe.
  post1 <- as.data.frame(posterior_unif)
  
  # Return list with posterior and credibility intervals.
  return(list(post1 = post1, ci95_unif = ci95_unif))
}

abc_graph <- function(data, title) {
  #'@title ABC Graph
  #'@author Stephen Pierzchajlo
  #'@description Plots posterior distribution and 95% CI.
  #'@usage abc_graph(data, title)
  #'@param data list containing n elements.
  #'@param title title to go above graph.
  #'@details Takes 1st list element of draws() output (posterior draws), and plots histogram on 
  #'x-axis. Then it takes the 1st and 2nd element of the second list element as the lower and upper
  #'estimates of the 95% CI and plots a line along the histogram.
  ggplot(data = data[[1]], aes(x = posterior_unif)) +
    geom_histogram(bins = 100, color = "darkblue", fill = "lightblue") +
    xlim(c(0, 1)) +
    geom_segment(aes(x = data[[2]][1], xend = data[[2]][2], y = 0, yend = 0, color = "red"),
                 size = 2) +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
          legend.position = "none")
  }