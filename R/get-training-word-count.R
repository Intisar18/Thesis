
# Get combined word counts of training data
# Returned data has columns
# word: word in Tweet
# n: count of words
get_training_word_count <- function(train_df){

	# Tidy
	tidy_df <- tidy_tweets(train_df)

	# Combine word counts
	word_count <-
		tidy_df %>%
		group_by(word) %>%
		count()

	message("Removing @ tags")
	# Remove @'s
	word_count <-
		word_count %>%
		dplyr::filter(!grepl("@", word)) # remove wods with an @
			
    message("Removing stop words")
	word_count <-
		word_count %>%
		anti_join(tidytext::stop_words)


	message("Removing numbers")
	# Remove digits
	word_count <-
		word_count %>%
		ungroup %>%
		mutate(
			word = stringr::str_replace(word,
			"[[:digit:]]+",
		 	stringr::fixed(""))) %>%
		dplyr::filter(word != "")

	word_count
}
