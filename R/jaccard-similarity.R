# Compute the weighted Jaccard value 

# c) Check similarity with other tweet 


jaccard_similarity <- function(words1, words2, intensity){

	which_intersect <- 
		map(words1, function(x) x %in% words2) 

	intersection_count <- reduce(which_intersect, sum)

	union_count <- 
		c(words1, words2) %>%
		unique() %>% 
		length()  
                    

	res <- intensity * (intersection_count/union_count)
	res
}



# Assume tweet is in word format?
jaccard_kernel_score <- function(tweet_words, training_words){

	# Unique id's and intensities
	ID_intensity <- 
		training_words %>%
		select(ID, `Intensity Score`) %>% 
		distinct()  

    # Get intensity and IDs
	intensity <- ID_intensity$`Intensity Score`
	IDs <- ID_intensity$ID 

	# Number of unique tweets in training data 
	n_train = length(IDs)

	# Unique list of words
	tweet_word_list <- map(IDs, 
		function(x) get_tweet_words(training_words, x))

	# Jaccard values
	jaccard_values <- 
		map(1:length(tweet_word_list), 
			function(x) jaccard_similarity(tweet_words, tweet_word_list[[x]], intensity[x]))

	weighted_value <- 
		jaccard_values %>%
		unlist() %>% 
		mean()

	weighted_value 
}



get_tweet_words <- function(word_tbl, word_id) {
	
	tweet_words <- 
		dplyr::filter(word_tbl, ID == word_id) %>% 
		dplyr::select(word) %>% 
		distinct()
	
	tweet_words$word 
}
