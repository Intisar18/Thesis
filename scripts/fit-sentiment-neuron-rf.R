# -----------------------------------------------------------------------------
# This script fits a random forest model on the 
# Sentiment-neuron features based on the OpenAI model. 
# See: https://github.com/openai/generating-reviews-discovering-sentiment
# -----------------------------------------------------------------------------

library(feather) 
library(readr)
library(purrr)
library(broom)
library(dplyr)
library(glmnet)
library(magrittr)
library(randomForest)
library(tibble)
library(stringr)

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

# Read in all the datasets 
all_data <- lapply(emotions, get_data_set) 

# Get test data with intensity score
test_sets <- lapply(emotions, 
				   get_data_set, 
				   data_dir = 'semeval-test-data', 
				   data_names = 'test') 

test_sets <- unlist(test_sets, recursive = FALSE)


# Name the data sets y the emotions
names(all_data) <- emotions 
names(test_sets) <- emotions

# Get training sets
train_sets <- purrr::map(all_data, ~.x$train )
dev_sets   <- purrr::map(all_data, ~.x$dev )
# test_sets   <- purrr::map(all_data, ~.x$test) 

# -----------------------------------------------------------------------------
# Get pathnames for sentiment data
# -----------------------------------------------------------------------------

folder = "sentiment-neuron-data"

filenames = dir(folder)
pathnames = paste0(file.path(folder, filenames))

# Just get the .txt pathnames 
pathnames <- pathnames[str_detect(pathnames, fixed(".txt"))]

is_train <- str_detect(pathnames, fixed("train.txt"))
is_dev <- str_detect(pathnames, fixed("dev.txt"))
is_test <- str_detect(pathnames, fixed("test.txt"))


# Read in files
sent_files <- map(pathnames[is_train], read_csv)

# Separate train
train_sent <- sent_files[is_train]
dev_sent <- sent_files[is_dev]
test_sent <- sent_files[is_test]


# train_sent <- map(pathnames[is_train], read_csv)
# dev_sent <- map(pathnames[is_dev], read_csv)
# test_sent <- map(pathnames[is_test], read_csv)


# names(train_sent) <- emotions
# names(train_y) <- sort(emotions)

# -----------------------------------------------------------------------------
# Join Intensity for training data
# -----------------------------------------------------------------------------

train_y <- map(train_sets, ~dplyr::pull(.x, `Intensity Score`))
test_y <- map(test_sets ~dplyr::pull(.x, `Intensity Score`))
dev_y <- map(dev_sets, ~dplyr::pull(.x, `Intensity Score`))

map(train_y, length) 
map(train_sent, dim)

# Re-sort train sent to match train_y
train_sent <- train_sent[c(1,3,4,2)]
test_sent <- test_sent[c(1,3,4,2)]
dev_sent <- dev_sent[c(1,3,4,2)]

names(train_sent) <- emotions
names(test_sent) <- emotions
names(dev_sent) <- emotions


all_data <- list(train = train_sent, test = test_sent)

map(train_y, length) 
map(train_sent, dim)

# -----------------------------------------------------------------------------
# Prep data with response, fix row number column
# -----------------------------------------------------------------------------

prep_data <- function(emotion, dataset){

	# Remove first column
	all_data[[dataset]][[emotion]] <- 
		dplyr::select(all_data[[dataset]][[emotion]], -X1)

	# Add response y to dataset 
	all_data[[dataset]][[emotion]]$y <- train_y[[emotion]]

	all_data[[dataset]][[emotion]]
}

train_data <- map(emotions, ~prep_data(.x, "train"))
names(train_data) <- emotions 

test_data <- map(emotions, ~prep_data(.x, "test"))
names(test_data) <- emotions


# -----------------------------------------------------------------------------
# Fit the random forest models
# -----------------------------------------------------------------------------

# Fit random forest
rf_fits <- 
	purrr::map2(train_sent, train_y, ~randomForest(.x, .y))

sentiment_neuron_rf_fits <- rf_fits 

# Save models 
saveRDS(sentiment_neuron_rf_fits, 
	file.path("data", "sentiment_neuron_rf_fits.RDS")) 


# -----------------------------------------------------------------------------
# Prediction
# -----------------------------------------------------------------------------

predict_rf_emotion <- function(emotion, xy){

	fit <- rf_fits[[emotion]]
	# y <- xy[[emotion]]$y
	# x <- train_sent[[emotion]] 
	x <- dev_sent[[emotion]]

	predict(fit, newdata=x)
}


predict_sent <- map(emotions, predict_rf_emotion)
predict_sent_dev <- map(emotions, predict_rf_emotion)

# -----------------------------------------------------------------------------
# Get pearson's correlation
# -----------------------------------------------------------------------------

map2(predict_sent, train_y, ~cor(.x, .y))
map2(predict_sent_dev, dev_y, ~cor(.x, .y))





