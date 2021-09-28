interaction_data <- function(dataframe) {

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