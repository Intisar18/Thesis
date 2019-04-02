# Baseline model on all data 

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
library(broom)
libray(dplyr)
library(glmnet)
library(magrittr)
library(randomForest)
library(tibble)

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")
# pal <- c("firebrick2", "orchid3", "steelblue3", "goldenrod3")
# names(pal) <- emotions

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
# Lasso test prediction
# -----------------------------------------------------------------------------

# alpha=1 is lasso penalty
# nlambda default is 100 

# Test 
# fit_lasso <- 
# 	glmnet(
# 		x=anger_x, 
# 		y=train_xy$anger$y,
# 		alpha = 1
# 		)

# log(anger_cv$lambda.1se)

# par(mfrow = c(1,2))
# plot(fit_lasso, xvar="lambda")
# plot(anger_cv)


# Paralellize 
library(doMC)
registerDoMC(cores=4)

# -----------------------------------------------------------------------------
# Fit lasso model
# -----------------------------------------------------------------------------

fit_lasso_emotion <- function(xy){
	x_mat <- as.matrix(xy$x)   # Lasso requires a matrix
	x_mat <- apply(x_mat, 2, as.numeric)

	# Cross validation of the lasso 
	cv.glmnet(
		x=x_mat,     # Text features/ 
		y=xy$y,      # Emotional intensity
		alpha = 1,
		type.measure = "mse", 
		parallel=TRUE)

}

lasso_fits <- purrr::map(train_xy, fit_lasso_emotion) 

# names(lasso_fits)

# -----------------------------------------------------------------------------
# Prediction 
# -----------------------------------------------------------------------------
mse <- function(y_true, y_pred){

	if(length(y_true) != length(y_pred)){
		stop("Length of the input vectors does not match")
	}

	sum((y_true-y_pred)^2)/length(y_pred)
}


predict_lasso_emotion <- function(emotion){
	
	fit <- lasso_fits[[emotion]]
	# y <- xy[[emotion]]$y
	x <- test_xy[[emotion]]$x
	mat_x <- as.matrix(x) 
	mat_x <- apply(mat_x, 2, as.numeric) 

	predict(fit, s=fit$lambda.1se, newx=mat_x)
}


lasso_mse <- function(emotion){
	y <- test_xy[[emotion]]$y
	mse(y, lasso_pred[[emotion]])
}


# Make predictiosn for all datasets 
lasso_pred <- purrr::map(emotions, predict_lasso_emotion) 

names(lasso_pred) <- emotions


mse <- purrr::map(emotions, lasso_mse)

names(mse) <- emotions 

test_mse_lasso_baseline <- mse 

saveRDS(test_mse_lasso_baseline, 
	file.path("results", "test_mse_lasso_baseline.RDS")) 


# -----------------------------------------------------------------------------
# Test upload
# -----------------------------------------------------------------------------

# readRDS(file.path("results", "test_mse_lasso_baseline"))


# test_mse_rf_baseline <- 
# 	readRDS(file.path("results", "test_mse_rf_baseline"))


