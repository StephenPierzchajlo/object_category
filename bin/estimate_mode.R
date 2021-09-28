# Function to estimate the mode of a distribution
estimate_mode <- function(x) {
  d <- density(x)
  d$x[which.max(d$y)]
}


draws <- function(n_success) {
# Number of draws
n_draws <- 5e5

# Data (proportion of successful participants, because a posterior is easy to compute)
n_participants <- 48
n_success <- n_success
success_rate <- n_success / n_participants

#######################################################################################
# Non-informed (uniform distribution)
prior_unif <- runif(n_draws, min = 0, max = 1)
# hist(prior_unif, breaks = 100)

# Posterior (with non-informed prior)
a <- rbinom(n = n_draws, size = n_participants, prob = prior_unif) # generates n_draws
# of data with 
# n_participants

posterior_unif <- prior_unif[a == n_success] # select draws where n_success was
# generated

length(posterior_unif) # Should be at least 5000 (make n_draws larger if not)

# Summarize/visualize the posterior
(maxL_unif <- estimate_mode(posterior_unif)) # the mode is the "Maximum likelihood
# estimate"

(ci95_unif <- quantile(posterior_unif, c(0.025, 0.975))) # the 95% credible interval

post1 <- as.data.frame(posterior_unif)

ci95_unif <- ci95_unif

return(list(post1 = post1, ci95_unif = ci95_unif))

}

abc_graph <- function(data) {
  ggplot(data = data[[1]], aes(x = posterior_unif)) + geom_histogram(bins = 100, color="darkblue", fill="lightblue") +
    xlim(c(0,1)) +
    geom_segment(aes(x=data[[2]][1], xend=data[[2]][2], y=0,yend=0, color = "red"), size = 2) +
    ggtitle("Participant is guessing") +
    theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"), legend.position = "none")
  
}