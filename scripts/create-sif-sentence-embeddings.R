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
# test_sets <- purrr::map(all_data, ~.x$test )

# Get test data with intensity score
test_sets <- lapply(emotions, 
				   get_data_set, 
				   data_dir = 'semeval-test-data', 
				   data_names = 'test') 

test_sets <- unlist(test_sets, recursive = FALSE)

names(test_sets) <- emotions

# -----------------------------------------------------------------------------
# Tidy training data
# -----------------------------------------------------------------------------

# Get tidy version of the training sets -- so one column is word 
# and another column is counts. 
tidy_train <- purrr::map(train_sets, tidy_tweets)
tidy_test <- purrr::map(test_sets, tidy_tweets)

names(tidy_test) <- emotions 

# Join the training data sets to the word-vector-probability data
tidy_joined <- 
	purrr::map(tidy_train,
		 function(x) dplyr::inner_join(x, word_df, by="word"))


# Join the training data sets to the word-vector-probability data
tidy_joined_test <- 
	purrr::map(tidy_test,
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

tidy_joined_test <- 
	purrr::map(tidy_joined_test, 
		function(x) mutate(x, weight = (n*alpha)/(alpha * probability)))


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

calculate_sentence_embedding <- function(emotion){

	data <- tidy_joined[[emotion]]
	# Split the data by tweet ID
	data_nested <- 
		data %>% 
		group_by(ID) %>% 
		nest()

	# Calculate the embedding for each tweet
	data_sent <- 
		data_nested$data %>% 
		purrr::map(get_tweet_sentence_embedding)  %>% # get embedding
		do.call(rbind,.) %>%     # Make results into a matrix 
		as_tibble()               # Make matrix a table

}

calculate_sentence_embedding_test <- function(emotion){

	data <- tidy_joined_test[[emotion]]
	# Split the data by tweet ID
	data_nested <- 
		data %>% 
		group_by(ID) %>% 
		nest()

	# Calculate the embedding for each tweet
	data_sent <- 
		data_nested$data %>% 
		purrr::map(get_tweet_sentence_embedding)  %>% # get embedding
		do.call(rbind,.) %>%     # Make results into a matrix 
		as_tibble()               # Make matrix a table

}

#  Get train sentence embeddings
sentence_embedding <- 
 	purrr::map(emotions, calculate_sentence_embedding)

# Get test sentence embeddings
sentence_embedding_test <- 
 	purrr::map(emotions, calculate_sentence_embedding_test)



names(sentence_embedding) <- emotions 
names(sentence_embedding_test) <- emotions 

# -----------------------------------------------------------------------------
# Get the first principal component of the sentence matrix
# -----------------------------------------------------------------------------

remove_principal_component <- function(sentence_embedding, emotion){

	sentence_data <- sentence_embedding[[emotion]]

	sentence_mat <- as.matrix(sentence_data)

	# Make sentence vectors the columns
	sentence_mat <- t(sentence_mat)

	# Create principal component decomposition
	sentence_pc <- svd(sentence_mat)

	# Get the first principal component/singular vector
	sentence_sv <- as.matrix(sentence_pc$v[1, ])

	#  Subtract the projection of the sentence onto the first singular
	# vector from the sentence vector
	remove_sv <- function(x){
	x - sentence_sv %*% t(sentence_sv) %*% x

	}

	# Remove singular vector from all sentence embeddings
	result <- apply(sentence_mat, 2, remove_sv)

	# Transpose the results 
	t(result)
}


sentence_mat <- 
	purrr::map(emotions, 
		~remove_principal_component(sentence_embedding, .x))

sentence_mat_test <- 
	purrr::map(emotions, 
		~remove_principal_component(sentence_embedding_test, .x))

names(sentence_mat) <- emotions
names(sentence_mat_test) <- emotions

# -----------------------------------------------------------------------------
# Check results
# -----------------------------------------------------------------------------

# sentence_mat[[1]] %>% dim

# -----------------------------------------------------------------------------
# Add Tweet Id back to data 
# -----------------------------------------------------------------------------

# Make data into a data frame and add back Tweet ID's 
sentence_df <- purrr::map(sentence_mat, ~as_tibble(.x))

# Make data into a data frame and add back Tweet ID's 
sentence_df_test <- purrr::map(sentence_mat_test, ~as_tibble(.x))

names(sentence_df) 
names(sentence_df_test) <- emotions 

# Get tweet Id's  
tweet_ID <- purrr::map(tidy_joined, ~unique(.x$ID))

# Get tweet Id's  
tweet_ID_test <- purrr::map(tidy_joined_test, ~unique(.x$ID))


add_ID <- function(emotion){
	sentence_data <- sentence_df[[emotion]]
	sentence_data$ID <- tweet_ID[[emotion]]
	sentence_data
}


add_ID_test <- function(emotion){
	sentence_data_test <- sentence_df_test[[emotion]]
	sentence_data_test$ID <- tweet_ID_test[[emotion]]
	sentence_data_test
}

sentence_df <- purrr::map(emotions, add_ID)
names(sentence_df) <- emotions

sentence_df_test <- purrr::map(emotions, add_ID_test)
names(sentence_df_test) <- emotions



# -----------------------------------------------------------------------------
# Add Intensity to data
# -----------------------------------------------------------------------------

# We are missing a few tweets in our data so we need to 
# join the intensity for the the tweets that match our data 
# train_ID_Intensity <- map(train_sets, 
# 	~dplyr::select(ID, `Intensity Score`))


# train_ID_Intensity_test <- map(test_sets, 
# 	~dplyr::select(ID, `Intensity Score`))


names(test_sets) <- emotions

# Join the training data to get the Intensity Score
sentence_df <- 
	map(emotions, 
		function(emotion) {
			# Join the intensity 
			left_join(sentence_df[[emotion]], train_sets[[emotion]], 
				by = c("ID")) %>%
		    # Remove the Tweet and Affect 
			dplyr::select(-Tweet, -`Affect Dimension`)
			})


sentence_df_test <- 
	map(emotions, 
		function(emotion) {
			# Join the intensity 
			left_join(sentence_df_test[[emotion]], test_sets[[emotion]], 
				by = c("ID")) %>%
		    # Remove the Tweet and Affect 
			dplyr::select(-Tweet, -`Affect Dimension`)
			})



# Rename
sentence_sif_df <- sentence_df 
sentence_sif_df_test <- sentence_df_test 

saveRDS(sentence_sif_df, file.path("data", "sentence_sif_df.RDS"))
saveRDS(sentence_sif_df_test, file.path("data", "sentence_sif_df_test.RDS"))




