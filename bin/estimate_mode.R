# Function to estimate the mode of a distribution
estimate_mode <- function(x) {
  
  #'@title estimate mode.
  #'@author Stephen Pierzchajlo
  #'@description Estimates the mode of a distribution.
  #'@usage estimate_mode(x)
  #'@param x This is a distribution whose mode is to be analyzed.
  #'@details This function returns a single value: The mode of the distribution used.
  
  d <- density(x)
  d$x[which.max(d$y)]
  }

draws <- function(n_success) {
  
  #'@title draws: draw random values.
  #'@author Stephen Pierzchajlo
  #'@description Produces posterior distribution for binomial model.
  #'@usage draws(n_success)
  #'@param n_success This function specifically draws n_success out of 48 draws.
  
  # Number of draws
  n_draws <- 5e5
  
  # Data (proportion of successful participants, because a posterior is easy to compute).
  n_participants <- 48
  n_success <- n_success
  success_rate <- n_success / n_participants
  
  # Non-informed (uniform distribution).
  prior_unif <- runif(n_draws, min = 0, max = 1)

  # generates n_draws of data with n_participants.
  a <- rbinom(n = n_draws, size = n_participants, prob = prior_unif)

  # select draws where n_success was generated.
  posterior_unif <- prior_unif[a == n_success]

  # Should be at least 5000 (make n_draws larger if not)
  length(posterior_unif)
  
  # Summarize/visualize the posterior.
  (maxL_unif <- estimate_mode(posterior_unif))
  
  # the 95% credible interval.
  (ci95_unif <- quantile(posterior_unif, c(0.025, 0.975))) 

  # Make dataframe.
  post1 <- as.data.frame(posterior_unif)

  # Define 95% CI
  ci95_unif <- ci95_unif
  
  return(list(post1 = post1, ci95_unif = ci95_unif))
  }

abc_graph <- function(data) {
  
  #'@title Plot adaptive bayesian computation posterior distribution.
  #'@author Stephen Pierzchajlo
  #'@description plots the output of theoretical adaptive bayesian computation models. Can directly
  #'feed draws() function into this function.
  #'@usage abc_graph(data)
  #'@param data a number. Can be the result of n_draws(), or even the function n_draws().
  
  # Produce posterior graph.
  ggplot(data = data[[1]], aes(x = posterior_unif)) +
    geom_histogram(bins = 100, color = "darkblue", fill = "lightblue") +
    xlim(c(0, 1)) +
    geom_segment(aes(x = data[[2]][1], xend = data[[2]][2], y = 0,yend = 0, color = "red"), size = 2) +
    ggtitle("Participant is guessing") +
    theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
          legend.position = "none")
  }