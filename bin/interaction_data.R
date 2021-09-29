interaction_data <- function(dataframe) {

  #'@title interaction data: dataframe shaper for interaction plotting.
  #'@author Stephen Pierzchajlo
  #'@description Takes dataframe and calculates incongruent - congruent difference for plotting.
  #'@usage interaction_data(dataframe).
  #'@param dataframe The data to be plotted.
  #'@details The dataframe must have a column called "congruency" that must only contain the catagories
  #'"congruent" and "incongruent". The function will split the data into two dataframes containing only
  #'congruent or incongruent trials, and then bind them side-by-side (i.e. by column). Then it will
  #'calculate the difference and save that as "diff". The output is useful for graphing the interaction
  #'between congruency and modality.
  
  # Create dataframe called "Congruent", which only contains congruent trials.
  congruent <- dataframe[dataframe$congruency == "congruent", ]

  # Rename columns in Congruent dataframe.
  colnames(congruent) <- c(
    "participant_c", "cue_type_c", "modality_c", "congruency_c", "reaction_time_c"
    )

  # Create dataframe called "Incongruent", which only contains incongruent trials.
  incongruent <- dataframe[dataframe$congruency == "incongruent", ]

  # Rename columns in Incongruent dataframe
  colnames(incongruent) <- c(
    "participant_i", "cue_type_i", "modality_i", "congruency_i", "reaction_time_i"
    )

  # Bind Congruent and Incongruent dataframes by column and call new dataframe "Difference".
  diff <- cbind(congruent, incongruent)

  # Create new column in Difference dataframe called "IncongruentMinusCongruent which
  # contains congruent and incongruent reaction time differences.
  diff$incongruent_minus_congruent <- diff$reaction_time_i - diff$reaction_time_c

  return(diff)
}