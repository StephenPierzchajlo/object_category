coloring <- function(dataframe) {
  # Indices.
  a <- 1
  b <- 2
  c <- 0
  # Loop.
  for (i in 1:length(levels(dataframe$participant))) {
    if (dataframe$reaction_time[[c + 1]] - dataframe$reaction_time[[c + (1 + 2)]] < 0) {
      dataframe$coloring[[c + 1]] = "down"
    } else {
      dataframe$coloring[[c + 1]] = "up"
    }
    if (dataframe$reaction_time[[c + 1]] - dataframe$reaction_time[[c + (1 + 2)]] < 0) {
      dataframe$coloring[[c + 3]] = "down"
    } else {
      dataframe$coloring[[c + 3]] = "up"
    }
    if (dataframe$reaction_time[[c + 2]] - dataframe$reaction_time[[c + (2 + 2)]] < 0) {
      dataframe$coloring[[c + 2]] = "down"
    } else {
      dataframe$coloring[[c + 2]] = "up"
    }
    if (dataframe$reaction_time[[c + 2]] - dataframe$reaction_time[[c + (2 + 2)]] < 0) {
      dataframe$coloring[[c + 4]] = "down"
    } else {
      dataframe$coloring[[c + 4]] = "up"
    }
    c <- c + 4
  }
  return(dataframe)
}