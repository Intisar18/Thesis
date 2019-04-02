library(dplyr)
library(tidytext)
library(stringr)#
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
# Get sentiment features used for filtering data
# -----------------------------------------------------------------------------

# Get nrc features for words
nrc_df <- get_nrc_features(word_count$word)

# Join words and sentiment values
word_sent <- cbind(word_count, nrc_df)

# -----------------------------------------------------------------------------
# Exploration of the data
# -----------------------------------------------------------------------------
# Look at which words have low or no sentiment values

# Get sentiment analysis data
sent_sum <- apply(nrc_df, 1, sum)

# 
zero_sent <- (sent_sum == 0)

# Just words with no sentiment
no_sent_df <- word_sent[zero_sent, ]

# About 4400 words have no sentment features

# -----------------------------------------------------------------------------
# Get hashtag words so word vecs can be retrieved separately
# -----------------------------------------------------------------------------

# which words are hashtags?
hashtag_df <-
	word_sent %>%
	dplyr::filter(grepl("#[a-z]+", word)) %>%
	mutate(hashword = word) %>%
	mutate(word = str_replace(hashword, fixed("#"), fixed("")))


# Check what the sum of the sentiment for the hashtags 
sent_hash_sum <- apply(hashtag_df[, 3:24], 1, sum)
zero_hash_sent <- (sent_hash_sum == 0)

# -----------------------------------------------------------------------------
# Add word vector information
# -----------------------------------------------------------------------------
# We add the word vector information to compare which words 
# are missing from the word vectors. The word vectors will be used 
# for clustering so we need the words to also be in the fastText dictionary.

# Load fastText sub word vectors/word embeddings
word_vec <- get_fastText_word_vectors()

hashtag_vec <- dplyr::left_join(hashtag_df, word_vec, by = "word")

# Which are all NA
not_complete <- !complete.cases(hashtag_vec)

# Get hashtags that have both features
hashtag_vec <- hashtag_vec[!(not_complete | zero_hash_sent), ]

# Replace word with original hashtag word
hashtag_vec$word <- hashtag_vec$hashword
hashtag_vec$hashword <- NULL


# -----------------------------------------------------------------------------
# Combine word vectors
# -----------------------------------------------------------------------------
# Filter words that are either missing from the FastText dictionary 
# or for which there are no sentiment features. 
# If either of these are missing, then we can not use the words for 
# clustering.

# Get word embeddings
word_feats <- dplyr::left_join(word_sent, word_vec, by = "word")

# Remove hashtags from word-feats dataframe
word_feats <-
	word_feats %>%
	dplyr::filter(!grepl("#[a-z]+", word))

# Get sentiment analysis data
sent_sum <- apply(word_feats[ ,3:24], 1, sum)
zero_sent <- (sent_sum == 0)

# Which are all NA
not_complete <- !complete.cases(word_feats)

# About 5000 words were not in the word_ved dictionary
# (which has a million words)

# There is a large overlap between the two sets
both_missing <- (not_complete & zero_sent)

sum(both_missing)

either_missing <- (not_complete | zero_sent)

sum(either_missing)

# Get word feats with no missing features
word_feats_final <- word_feats[!either_missing, ]

# Add hashtags
word_feats_final <- rbind(hashtag_vec, word_feats_final)

# The size of the complete data set
dim(word_feats_final)

# 8343 words with 322 features

# Remove word_vec to free memory
rm(word_vec)

# -----------------------------------------------------------------------------
# Filtering: remove words with low sentiment values
# -----------------------------------------------------------------------------
# Reduce the number of words in the feature set by removing words 
# that don't have much sentiment value 

# Sum the absolute value of the sentiment lexicons
sent_final_sum <- apply(abs(word_feats_final[ ,3:24]), 1, sum)

# Whats the distribution
hist(sent_final_sum, breaks = seq(0, 16, by = 0.5))

# Set the filtering critera
is_boring <- (sent_final_sum < 5.5)  # "test" value
# is_boring <- (sent_final_sum < 3.0)
# is_boring <- (sent_final_sum < 1.5)

# Which words are boring
word_feats_not_boring <- word_feats_final[!is_boring, ]

dim(word_feats_not_boring)

# 7500 words final
test <-
word_feats_not_boring %>%
	group_by(word)


word_features <-
	word_feats_not_boring %>%
	distinct(word, .keep_all = TRUE)


# saveRDS(word_features, file.path("data", "word_features.RDS"))
# saveRDS(word_features, file.path("data", "word_features_3_0.RDS"))
saveRDS(word_features, file.path("data", "word_features_5_5.RDS"))

# saveRDS(word_features, file.path("data", "word_features_test.RDS"))


# -----------------------------------------------------------------------------
# Save word cluster and word distance matrix
# -----------------------------------------------------------------------------
word_dist <- dist(word_features[ ,3:324], method="euclidean")

word_clust <- hclust(word_dist, method="complete")


# saveRDS(word_dist, file.path("data", "word_dist.RDS"))
# saveRDS(word_clust, file.path("data", "word_clust.RDS"))

# saveRDS(word_dist, file.path("data", "word_dist_3_0.RDS"))
# saveRDS(word_clust, file.path("data", "word_clust_3_0.RDS"))

saveRDS(word_dist, file.path("data", "word_dist_5_5.RDS"))
saveRDS(word_clust, file.path("data", "word_clust_5_5.RDS"))

# saveRDS(word_dist, file.path("data", "word_dist_test.RDS"))
# saveRDS(word_clust, file.path("data", "word_clust_test.RDS"))
