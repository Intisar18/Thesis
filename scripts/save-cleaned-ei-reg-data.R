# Load R files 

library(dplyr)
library(purrr) 
library(readr)
library(tidytext)	# Unnest tokens and tokenize tweets
library(tm) 		# Document term matrix

# This is a temporary method for loading functions from the R 
# folder


#-----------------------------------------------------
#  Source() R functions 
#-----------------------------------------------------

R_files <- file.path("R", list.files("R"))

purrr::map(R_files, source)

data_files <- get_data_file_names()


#-----------------------------------------------------
#  Get file types for each emotion
#-----------------------------------------------------

# Make a list of all emotion types
emotions <- c("anger", "fear", "joy", "sadness")

file_prefix <- "EI-reg-En"

ei_reg_file_types <- paste0(file_prefix, "-", emotions)



emotion_file_prefix <- "EI-reg-En-joy"
# Function to read in all data for an emotion

read_emotion_data <- function(emotion_file_prefix) {

	# Read in anger only motional intensity regression data
	ei_reg_emotion <- read_task_files(emotion_file_prefix, data_files)


	dataset_types <- c("dev", "test", "train")

	# Data is in order (dev, test, train)
	names(ei_reg_emotion) <- dataset_types 

	ei_reg_emotion
}




all_emotion_data <- purrr::map(ei_reg_file_types, read_emotion_data)

names(all_emotion_data) <- emotions




#------------------------------------------------------
# Column names used in data frames
#------------------------------------------------------
# * ID: tweet ID 
# * Tweet: tweet text 
# * Affect Dimension: Emotion type
# * Intensity : Emotion intensity (y- value) 
# * clean_tweet: Then tweet with URL's stripped 
# * term : The column of tokens for each tweet 
# * count : The count of each token per docuent
#------------------------------------------------------

#------------------------------------------------------
# Clean data
#------------------------------------------------------


# Function to clean urls 
remove_urls <- function(input) {

	# remove http elements manually	
	result <- gsub("http.*","",  input)
	result <- gsub("https.*","", result)
	return(result)
}

clean_tweet_data <- function(data_train) {
	# Remove urls
	data_train <- 
		data_train %>% 
		mutate(clean_tweet = remove_urls(Tweet))

	# Use unnest_token to tokenize tweets
	data_words <- 
		data_train %>%
		unnest_tokens(word, clean_tweet, token="tweets") 

	# Remove stop words 
	data_words <- 
		data_words %>% 
		anti_join(stop_words, by = c("word" = "word"))

	data_words 
}


# Clean for each emotion
anger_cleaned <- purrr::map(all_emotion_data$anger, clean_tweet_data) 
fear_cleaned <- purrr::map(all_emotion_data$fear, clean_tweet_data) 
joy_cleaned <- purrr::map(all_emotion_data$joy, clean_tweet_data) 
sadness_cleaned <- purrr::map(all_emotion_data$sadness, clean_tweet_data) 


make_file_names <- function(emotion) {

	dataset_types <- c("dev", "test", "train")
	paste0(emotion, "_", dataset_types, "_", "clean.csv")

}

emotion_file_names <- purrr::map(emotions, make_file_names)
names(emotion_file_names) <- emotions


# Get emotion names
anger_names <- emotion_file_names$anger
fear_names <- emotion_file_names$fear
joy_names <- emotion_file_names$joy
sadness_names <- emotion_file_names$sadness


# Save files to cleaned-data folder
purrr::map(1:3, 
	function(x) write_csv(anger_cleaned[[x]], file.path("cleaned-data", anger_names[x]))
	)

purrr::map(1:3, 
	function(x) write_csv(fear_cleaned[[x]], file.path("cleaned-data", feat_names[x]))
	)

purrr::map(1:3, 
	function(x) write_csv(joy_cleaned[[x]], file.path("cleaned-data", joy_names[x]))
	)

purrr::map(1:3, 
	function(x) write_csv(sadness_cleaned[[x]], file.path("cleaned-data", sadness_names[x]))
	)


