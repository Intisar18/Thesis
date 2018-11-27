# Create tidy data.frame from training data 


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

tidy_tweets <- function(tweets) {
	
	# Remove urls
	tweets <- 
		tweets %>% 
		mutate(clean_tweet = remove_urls(Tweet))

	# Use unnest_token to tokenize tweets
	tweets_words <- 
		tweets %>%
		unnest_tokens(word, clean_tweet, token="tweets") 

	# Remove stop words 
	tweets_words <- 
		tweets_words %>% 
		anti_join(stop_words, by = c("word" = "word"))

	# Count words by document  
	tweets_count <- 
		tweets_words %>% 
		group_by(ID) %>%
		count(word, sort = TRUE)

	tweets_count

}