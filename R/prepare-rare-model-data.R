
# Prepare tidy data to be fit for a rare model
# This returns augmented_data that contains all the words needed
# that are in the original model
prepare_rare_model_data <- function(tidy_data, word_features) {

	# Get startIDs in input
	start_ID <- unique(tidy_data$ID)

	# Get words in input
	start_words <- unique(tidy_data$word)

	target_words <- unique(word_features$word)

	stopifnot(length(target_words) == nrow(word_features))

	# Remove words in input not in feature
	retained_words = start_words[start_words %in% target_words]

	percent_kept <- length(retained_words)/length(start_words)
	message("Percent of words retained was ", round(percent_kept*100))

	retained_data <-
		tidy_data %>%
		dplyr::filter(word %in% retained_words)

	retained_ID <- unique(retained_data$ID)

	# Get missing ID list
	dropped_ID <- start_ID[!(start_ID %in% retained_ID)]

	percent_ID_dropped <- length(dropped_ID)/length(start_ID)

	message("Percent of training samples dropped is: ",
		round(percent_ID_dropped*100))

	# Get words in feature not in input
	missing_words <- target_words[!(target_words %in% retained_words)]

	# Create dummy tweet withS
	missing_df <-
	tibble(ID = rep("NO_TWEET", length(missing_words)),
		   word = missing_words,
		   n = 0)



	augmented_data <- dplyr::bind_rows(retained_data, missing_df)


	return( list("augmented_data" = augmented_data,
		"retained_ID" = retained_ID,
		"dropped_ID" = dropped_ID))

}
