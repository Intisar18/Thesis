library(purrr) 
library(dplyr)
library(readr)
library(ggplot2) 
library(tidytext)

# for rare model
library(rare)
library(Matrix)

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")
pal <- c("firebrick2", "orchid3", "steelblue3", "goldenrod3")
names(pal) <- emotions

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# Read in all the datasets 
all_data <- lapply(emotions, get_data_set) 

# Name the data sets y the emotions
names(all_data) <- emotions 

# Get training sets
training_sets <- purrr::map(all_data, ~.x$train )


# rename_tweet_columns <- function(data){
# 	names(data) <-  c("ID", "Tweet", "Affect", "Intensity")
# 	data
# }

# Get tidy data set for training data
tidy_train <- purrr::map(training_sets, tidy_tweets)

# Add column identifying emotion to each training set
tidy_train <- 
	purrr::map(1:length(emotions), function(idx) {
		tidy_train[[idx]]$emotion <- emotions[idx]; tidy_train[[idx]]
	})

# Combine to single data.frame
tidy_train <- do.call(rbind, tidy_train)

# Count by emotion 
tidy_count <- 
	tidy_train  %>% 
	dplyr::group_by(word, emotion ) %>%
	dplyr::summarise(count = sum(n)) %>% 
	dplyr::arrange(desc(count))



#------------------------------------------------------------------------------
# TOP WORD COUNT BY EMOTION
#------------------------------------------------------------------------------

# Plot by emotion 
tidy_count %>% 
	dplyr::filter(count > 20) %>%
	dplyr::filter(emotion == emotions[1]) %>%
	ggplot(aes(x = reorder(word, count), y = count, fill = emotion))+
	geom_col() + 
	scale_fill_manual(values = pal) + 
	coord_flip() + 
	facet_wrap(~emotion, nrow = 4) +  
	theme_minimal() 

tidy_count %>% 
	dplyr::filter(count > 20) %>%
	dplyr::filter(emotion == emotions[2]) %>%
	ggplot(aes(x = reorder(word, count), y = count, fill = emotion))+
	geom_col() + 
	scale_fill_manual(values = pal) + 
	coord_flip() + 
	facet_wrap(~emotion, nrow = 4) +  
	theme_minimal() 

# ggsave("images/joy-count.jpg")

tidy_count %>% 
	dplyr::filter(count > 20) %>%
	dplyr::filter(emotion == emotions[3]) %>%
	ggplot(aes(x = reorder(word, count), y = count, fill = emotion))+
	geom_col() + 
	scale_fill_manual(values = pal) + 
	coord_flip() + 
	facet_wrap(~emotion, nrow = 4) +  
	theme_minimal() 

# ggsave("images/sadness-count.jpg")

tidy_count %>% 
	dplyr::filter(count > 20) %>%
	dplyr::filter(emotion == emotions[4]) %>%
	ggplot(aes(x = reorder(word, count), y = count, fill = emotion))+
	geom_col() + 
	scale_fill_manual(values = pal) + 
	coord_flip() + 
	facet_wrap(~emotion, nrow = 4) +  
	theme_minimal() 

# ggsave("images/fear-count.jpg")

#------------------------------------------------------------------------------
# WORD DISTRIBUTION
#------------------------------------------------------------------------------

tidy_count %>% 
	# dplyr::filter(count > 1) %>%
	ggplot(aes(x = count, fill = emotion))+
	geom_histogram(bins = 100) + 
	scale_fill_manual(values = pal) + 
	# coord_flip() + 
	facet_wrap(~emotion, nrow = 2) +
	labs(title = "Word distribution by emotion") + 
	theme_minimal() 

# ggsave("images/word-emotion-distribution.png")


#------------------------------------------------------------------------------
# TF-IDF : PLOT IS NOT INTERESTING 
#------------------------------------------------------------------------------

# Get original tidy data 
tidy_train <- purrr::map(training_sets, tidy_tweets)


# tidy_train$anger %>% bind_tf_idf(word, ID, n)
# Combine to single data.frame
tidy_train <- do.call(rbind, tidy_train)

# Combine words before tf-idf
tidy_count <- 
	tidy_train %>% 
	dplyr::group_by(word, ID) %>% 
	dplyr::summarise(count = sum(n)) %>% 
	dplyr::arrange(desc(count))

#  Bind tidy together to look at overall tfidf
tidy_tfidf <- 
	tidy_count %>%
	tidytext::bind_tf_idf(word, ID, count) %>% 
	dplyr::arrange(desc(tf_idf))



# Plot by emotion 
tidy_tfidf %>% 
	dplyr::filter(tf_idf > 5) %>%
	# dplyr::filter(emotion == emotions[1]) %>%
	ggplot(aes(x = reorder(word, tf_idf), y = tf_idf))+
	geom_col() + 
	scale_fill_manual(values = pal) + 
	coord_flip() + 
	# facet_wrap(~emotion, nrow = 4) +  
	theme_minimal() 




