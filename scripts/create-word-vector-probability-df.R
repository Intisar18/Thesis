library(purrr) 
library(dplyr)
library(readr)
library(ggplot2) 
library(tidytext)

# Text features 
library(textfeatures)


# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Create set of words that will be used as features in rare model
# -----------------------------------------------------------------------------

# Combine all training data into one data set
train_df <- get_combined_training_df()

# Get cleaned word counts of trainind data
word_count <- get_training_word_count(train_df)

# -----------------------------------------------------------------------------
# Create dictionary of word | vector | probability
# -----------------------------------------------------------------------------

# Get the fast text word vectors
word_vec <- get_fastText_word_vectors()

# Join the word counts and word vectors into one dataframe
word_count_vec <-
	word_count %>% 
	dplyr::left_join(word_vec, by = "word")

# Compute the prorbaility of each word. We are doing this before 
#  we filter out values because it does not matter if the probability 
# really adds to 1.00
word_df <- 
	word_count_vec %>% 
	mutate(probability = nn/sum(nn))

# When we joined the word vectors NA rows were added if there was no 
# word in the word vector dictionary. Filter those NA's out
word_df <-
	word_df %>% 
	dplyr::filter(complete.cases(.))

# Remove the word count column. We just need the word vectors and probability
word_df <- 
	word_df %>% 
	dplyr::select(-nn)

# Rename 
word_vec_prob_df <- word_df 

# -----------------------------------------------------------------------------
# Save the word-vector-probability table
# -----------------------------------------------------------------------------

saveRDS(word_vec_prob_df, file.path("data", "word_vec_prob_df.RDS"))





