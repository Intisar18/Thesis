# Load R files 

library(dplyr)
library(purrr) 
library(readr)
library(tidytext)	# Unnest tokens and tokenize tweets
library(tm) 		# Document term matrix

# This is a temporary method for loading functions from the R 
# folder

R_files <- file.path("R", list.files("R"))

purrr::map(R_files, source)


data_files <- get_data_file_names()

# Read in anger only motional intensity regression data
ei_reg_anger <- read_task_files("EI-reg-En-anger", data_files)

# Check data dimensions 
purrr::map(ei_reg_anger, dim)

# Data is in order (dev, test, train)
names(ei_reg_anger) <- c("dev", "test", "train")

anger_train <- ei_reg_anger[["train"]]

# Check data 
dim(anger_train)
names(anger_train)

# Function to clean urls 
remove_urls <- function(input) {

	# remove http elements manually	
	result <- gsub("http.*","",  input)
	result <- gsub("https.*","", result)
	return(result)
}


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

# Remove urls
anger_train <- 
	anger_train %>% 
	mutate(clean_tweet = remove_urls(Tweet))

# Use unnest_token to tokenize tweets
anger_words <- 
	anger_train %>%
	unnest_tokens(word, clean_tweet, token="tweets") 

# Remove stop words 
anger_words <- 
	anger_words %>% 
	anti_join(stop_words, by = c("word" = "word"))

# Count words by document  
anger_count <- 
	anger_words %>% 
	group_by(ID) %>%
	count(word, sort = TRUE)

# Cast to document, term matrix. Rows are document Id's, columns are terms/words
anger_dtm <- 
	anger_count %>% 
  # document ID, term column, values
	cast_dtm(ID, word, n)



#--------------------------------------------------------
# Test jaccard kernel
#--------------------------------------------------------

# Using the training data set to test functions
training_words <- anger_words

ID1 = "2017-En-10264"
ID2 = "2017-En-10072"
ID3 = "2017-En-10946"

tweet_words1  <- get_tweet_words(training_words, ID1)
tweet_words2  <- get_tweet_words(training_words, ID2)
tweet_words3  <- get_tweet_words(training_words, ID3)



jaccard_kernel_score(tweet_words3, anger_words)


#--------------------------------------------------------
# Test jaccard similiarity
#--------------------------------------------------------

intensity = 0.02

# Use word table
jaccard_similarity(tweet_words1, tweet_words3, 0.4)