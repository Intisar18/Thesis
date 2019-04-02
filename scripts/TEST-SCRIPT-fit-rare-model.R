# -----------------------------------------------------------------------------
# From: test-rare-model
# -----------------------------------------------------------------------------

library(purrr) 
library(dplyr)
library(readr)
library(ggplot2) 

# for rare model
library(rare)
library(Matrix)

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# Read in all the datasets 
all_data <- lapply(emotions, get_data_set) 

# Name the data sets y the emotions
names(all_data) <- emotions 

# Get training sets
training_sets <- purrr::map(all_data, ~.x$train )
dev_sets <- purrr::map(all_data, ~.x$dev)


anger_train <- training_sets$anger 
anger_dev <- all_data$anger$dev 

names(anger_train) <- c("ID", "Tweet", "Affect", "Intensity")

t_anger_train <- tidy_tweets(anger_train)
t_anger_dev <- tidy_tweets(anger_dev)



# -----------------------------------------------------------------------------
# Filter for words that were in the tidy vec 
# -----------------------------------------------------------------------------
anger_filtered <-
t_anger_train %>% 
	dplyr::filter(word %in% tidy_vec$word) 

anger_dtm <- 	
anger_filtered %>%
  # document ID, term column, values
	cast_dtm(ID, word, n)


# -----------------------------------------------------------------------------
# Filter out words in tidy_vec not in the anger data
# -----------------------------------------------------------------------------

tidy_filtered <- 
	tidy_vec %>% 
	dplyr::filter(word %in% anger_filtered$word)


anger_dist <- 
	dist(tidy_filtered[ ,3:302], method="euclidean")


# Save data 
save(anger_dist, file = file.path("data", "anger_dist.RData"))

# Get remaining tweets 
ID_filtered <- 
anger_filtered$ID %>% 
	unique

anger_train_filtered <-
	anger_train %>% 
	dplyr::filter(ID %in% ID_filtered)

# Get response variable 
anger_intensity <- anger_train_filtered$Intensity

nrows = nrow(anger_dtm)

hc_anger <- hclust(anger_dist, method="complete")


# Sample from training data and build on sample
# set.seed(100)
# ts <- sample(1:nrows, nrows * .75) # Train set indices

ourfit <- 
rarefit(y = anger_intensity, 
	X = anger_dtm, 
	hc = hc_anger, 
	lam.min.ratio = 1e-6, 
	nlam = 20, 
	nalpha = 10, 
	rho = 0.01, 
	eps1 = 1e-5, 
	eps2 = 1e-5, 
	maxite = 1e4)


anger_fit <- ourfit

# Save data 
save(anger_fit, file = file.path("data", "anger_fit.RData"))

