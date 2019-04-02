# Baseline model on all data 

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
library(broom)
library(dplyr)
library(glmnet)
library(magrittr)
library(randomForest)
library(tibble)

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")
pal <- c("firebrick2", "orchid3", "steelblue3", "goldenrod3")
names(pal) <- emotions

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

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
# Load baseline features
# -----------------------------------------------------------------------------

basic_features <- readRDS(file.path("data", "basic_features.RDS"))
nrc_features <- readRDS(file.path("data", "nrc_features.RDS"))

basic_features_dev <- readRDS(file.path("data", "basic_features_dev.RDS"))
nrc_features_dev <- readRDS(file.path("data", "nrc_features_dev.RDS"))

basic_features_test <- readRDS(file.path("data", "basic_features_test.RDS"))
nrc_features_test <- readRDS(file.path("data", "nrc_features_test.RDS"))


# -----------------------------------------------------------------------------
# Join all data to make retrieving data from easier
# -----------------------------------------------------------------------------
train_data <- 
	list(data = train_sets, 
		basic = basic_features, 
		nrc = nrc_features)

dev_data <- 
	list(data = dev_sets, 
		basic = basic_features_dev, 
		nrc = nrc_features_dev)

test_data <- 
	list(data = test_sets, 
		basic = basic_features_test, 
		nrc = nrc_features_test)


all_data <- list(train = train_data, dev = dev_data, test=test_data)

# -----------------------------------------------------------------------------
# Prepare data for being fit 
# -----------------------------------------------------------------------------

prep_baseline_features <- function(emotion, dataset){

	dat = all_data[[dataset]]

	basic <- dat$basic[[emotion]]
	nrc <- dat$nrc[[emotion]]

	combined_features <- cbind(basic, nrc)

	y <- dat$data[[emotion]][["Intensity Score"]]	

	list(x = combined_features, y = y)
}


# -----------------------------------------------------------------------------
# Get data
# -----------------------------------------------------------------------------
train_xy <- lapply(emotions, prep_baseline_features, dataset = "train")

dev_xy <- lapply(emotions, prep_baseline_features, dataset="dev")

test_xy <- lapply(emotions, prep_baseline_features, dataset="test")

names(train_xy) <- emotions 
names(test_xy) <- emotions 
names(dev_xy) <- emotions 


# -----------------------------------------------------------------------------
# Fit models
# -----------------------------------------------------------------------------
rf_fits <- purrr::map(train_xy, 
				function(x) {
					randomForest(x$x, x$y)
				})

# -----------------------------------------------------------------------------
# Predict
# -----------------------------------------------------------------------------

mse <- function(y_true, y_pred){

	if(length(y_true) != length(y_pred)){
		stop("Length of the input vectors does not match")
	}

	sum((y_true-y_pred)^2)/length(y_pred)
}


predict_rf_emotion <- function(emotion, xy){

	fit <- rf_fits[[emotion]]
	# y <- xy[[emotion]]$y
	x <- xy[[emotion]]$x 
	
	predict(fit, newdata=x)
}


mse_rf_emotion <- function(emotion, pred, xy){

	pred <- pred[[emotion]]
	y <- xy[[emotion]]$y

	message("emotion: ", emotion)
	message("pred len: ", length(pred)) 
	message("y len: ", length(y))

	message("p: ", head(pred)) 
	message("y: ", head(y))
	
	mse <- sum((y-pred)^2)/length(y)
	mse
}

# -----------------------------------------------------------------------------
# Random forest test prediction
# -----------------------------------------------------------------------------

# Get random forest prediction
test_pred <- lapply(emotions, predict_rf_emotion, xy = test_xy)

names(test_pred) <- emotions 


# Calculate MSE
test_mse <- 
	lapply(emotions, mse_rf_emotion, pred=test_pred, xy=test_xy)
names(test_mse) <- emotions


test_mse_rf_baseline <- test_mse 

saveRDS(test_mse_rf_baseline, 
	file.path("results", "test_mse_rf_baseline.RDS")) 


