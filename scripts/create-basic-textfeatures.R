library(purrr) 
library(dplyr)
library(readr)
library(ggplot2) 
library(tidytext)

# Text features 
library(textfeatures)


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
train_sets <- purrr::map(all_data, ~.x$train )
dev_sets <- purrr::map(all_data, ~.x$dev )
test_sets <- purrr::map(all_data, ~.x$test )

# Get features from tweets (notice that we don't do any processing
# of the tweets before getting this features)
basic_features <- lapply(train_sets, 
	function(x) textfeatures(x$Tweet, word_dims=100))

basic_features_dev <- lapply(dev_sets, 
	function(x) textfeatures(x$Tweet, word_dims=100))
basic_features_test <- lapply(test_sets, 
		function(x) textfeatures(x$Tweet, word_dims=100))


# Get NRC lexicon features
nrc_features <- lapply(train_sets, function(x) get_nrc_features(x$Tweet))
nrc_features_dev <- lapply(dev_sets, function(x) get_nrc_features(x$Tweet))
nrc_features_test <- lapply(test_sets, function(x) get_nrc_features(x$Tweet))



# Save result

# saveRDS(nrc_features, file=file.path("data", "nrc_features.RDS"))
saveRDS(nrc_features_dev, file=file.path("data", "nrc_features_dev.RDS"))
saveRDS(nrc_features_test, file=file.path("data", "nrc_features_test.RDS"))

# test <- readRDS(file.path("data", "nrc_features.RDS"))

# lapply(test, dim)

saveRDS(basic_features, file=file.path("data", "basic_features.RDS"))
saveRDS(basic_features_dev, file=file.path("data", "basic_features_dev.RDS"))
saveRDS(basic_features_test, file=file.path("data", "basic_features_test.RDS"))

# test <- readRDS(file.path("data", "basic_features.RDS"))

# lapply(test, dim)



#----------------------------------------------------------
# Explore results 
#----------------------------------------------------------

# fear_features <- basic_features$fear

# # See if features are highly correlated
# library(GGally)

# ggpairs(fear_features[c(3, 8, 19:21)])

# # The bing and afinn sentiment are correlated 
# cor(fear_features[2:21])
