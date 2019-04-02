
get_nrc_features <- function(input_text) {

	# -------------------------------------------------------------------------
	# Get data.frame of lexicon features
	# -------------------------------------------------------------------------

	# bigrams <- readRDS(file.path("data", "bigram_lex.RDS") 
	hashtag_category <- readRDS(file.path("data", "hashtag_category.RDS")) 
	unigram_lex <- readRDS(file.path("data", "unigram_lex.RDS")) 
	# nrc_vad <- readRDS(file.path("data", "nrc_vad.RDS") 

	# ------------------------------------------------------------------------
	# NRC Features
	# -------------------------------------------------------------------------

	nrc_features <- syuzhet::get_nrc_sentiment(input_text)

	# Update names 
	names(nrc_features) <- paste0("nrc_", names(nrc_features))

	nrc_feature_df <- dplyr::as_tibble(nrc_features)

	# -------------------------------------------------------------------------
	# HASHTAG sentiment by categor
	# -------------------------------------------------------------------------

	hashtag_features <- 
		lapply(hashtag_category, 
			function(dict) syuzhet::get_sentiment(input_text, 
										method="custom", 
										lexicon=dict))
	# Update names 
	names(hashtag_features) <- paste0("hashtag_", names(hashtag_features) )

	# Create data.frame
	hashtag_feature_df <- do.call(cbind, hashtag_features) 

	# Change to tibble
	hashtag_feature_df <- dplyr::as_tibble(hashtag_feature_df)


	# -------------------------------------------------------------------------
	# UNIGRAM features
	# -------------------------------------------------------------------------

	# Get unigram features 
	unigram_features <- 
		lapply(unigram_lex,
			 function(dict) syuzhet::get_sentiment(input_text, 
			 							method="custom", 
			 							lexicon=dict))

	# Create data.frame
	unigram_feature_df <- do.call(cbind, unigram_features) 

	unigram_feature_df <- dplyr::as_tibble(unigram_feature_df)


	# -------------------------------------------------------------------------
	# Bind the features into a single data frame
	# -------------------------------------------------------------------------

	features_df <- 
		cbind(nrc_feature_df, hashtag_feature_df, unigram_feature_df)


	return(features_df)

}





# -----------------------------------------------------------------------------
# TEST
# -----------------------------------------------------------------------------

# my_example_text <- "I begin this story with a neutral statement.  
#   Basically this is a very silly test.  
#   You are testing the Syuzhet package using short, inane sentences.  
#   I am actually very happy today. 
#   I have finally finished writing this package.  
#   Tomorrow I will be very sad. 
#   I won't have anything left to do. 
#   I might get angry and decide to do something horrible.  
#   I might destroy the entire package and start from scratch.  
#   Then again, I might find it satisfying to have completed my first R package. 
#   Honestly this use of the Fourier transformation is really quite elegant.  
#   You might even say it's beautiful!"

# s_v <- get_sentences(my_example_text)


# get_nrc_features(s_v)




