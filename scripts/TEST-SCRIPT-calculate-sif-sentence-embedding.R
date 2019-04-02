library(purrr) 
library(dplyr)
library(readr)
library(ggplot2) 
library(tidytext)
library(tidyr)

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Read in word-vector-probability dataset
# -----------------------------------------------------------------------------

word_df <- readRDS(file.path("data", "word_vec_prob_df.RDS"))

# -----------------------------------------------------------------------------
# Read in training data
# -----------------------------------------------------------------------------

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")

# Read in all the datasets 
all_data <- lapply(emotions, get_data_set) 

# Name the data sets y the emotions
names(all_data) <- emotions 

# Get training sets
train_sets <- purrr::map(all_data, ~.x$train )
dev_sets <- purrr::map(all_data, ~.x$dev )
test_sets <- purrr::map(all_data, ~.x$test )

# -----------------------------------------------------------------------------
# Tidy training data
# -----------------------------------------------------------------------------

# Get tidy version of the training sets -- so one column is word 
# and another column is counts. 
tidy_train <- purrr::map(train_sets, tidy_tweets)

# Join the training data sets to the word-vector-probability data
tidy_joined <- 
	purrr::map(tidy_train,
		 function(x) dplyr::inner_join(x, word_df, by="word"))


# -----------------------------------------------------------------------------
# Calculate column equal to (a * count)/(a + p(w)) 
# -----------------------------------------------------------------------------
# Based on (see paper ref)
alpha = 0.0001; 

# The weighting scheme effectively down-weights the higher probability 
# words
tidy_joined <- 
	purrr::map(tidy_joined, 
		function(x) mutate(x, weight = (n*alpha)/(alpha * probability)))

# -----------------------------------------------------------------------------
# Example function for calculating sentence embeeding for one tweet
# -----------------------------------------------------------------------------

anger_data <- tidy_joined$anger

ID_1 <- anger_data$ID[1]

one_tweet <- 
	anger_data %>% 
	dplyr::filter(ID == ID_1)


# -----------------------------------------------------------------------------
# Calculate a weighted embedding for a data frame with a single tweet
# -----------------------------------------------------------------------------

get_tweet_sentence_embedding <- function(tweet_df){

	# Get weights as a vector
	weights <- tweet_df$weight

	# Get a matrix of the word vectors
	tweet_mat <- 
		tweet_df %>% 
		ungroup %>%
			dplyr::select(starts_with("V")) %>% 
			as.matrix

	# if(length(dim(tweet_mat)) > 0){ 

	# Get weighted word vector
	weighted_vec <- apply(tweet_mat, 2, function(x) x * weights)
	# Make a matrix to take care of single vectors 
	weighted_vec <- as.matrix(weighted_vec)
	
	# Average weighted word vector 
	result <- apply(weighted_vec, 2, 
		function(x) mean(x * weights, na.rm = TRUE))

	result
}



# -----------------------------------------------------------------------------
# Apply sentence embedding calculation to all tweets
# -----------------------------------------------------------------------------

# Split the data by tweet ID
anger_nested <- 
	anger_data %>% 
		group_by(ID) %>% 
		nest()

# Calculate the embedding for each tweet
anger_sent <- 
	anger_nested$data %>% 
	purrr::map(get_tweet_sentence_embedding)  %>% # get embedding
	do.call(rbind,.) %>%     # Make results into a matrix 
	as_tibble()               # Make matrix a table


# Assign the IDs from the nested table
anger_sent$ID <- anger_nested$ID 

