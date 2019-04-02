library(purrr)
library(dplyr)
library(readr)
library(ggplot2)
library(glmnet)

# for rare model
library(rare)
library(Matrix)

# Script prepares training and dev data to be fit using "rare" model


# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Load training and dev data
# -----------------------------------------------------------------------------
all_data <- get_all_data() 

# -----------------------------------------------------------------------------
# Choose the data set 
# -----------------------------------------------------------------------------
# word_features <- readRDS(file.path("data", "word_features.RDS"))
word_features <- readRDS(file.path("data", "word_features_3_0.RDS"))
# word_features <- readRDS(file.path("data", "word_features_5_5.RDS"))

# Get anger data
anger_res <- 
	prepare_train_dev_rare_model_data("anger", all_data, word_features) 

# Get DTM for each data set
train_dtm <- anger_res$train_dtm 
dev_dtm <- anger_res$dev_dtm 
test_dtm <- anger_res$test_dtm 

# Get emotion intensity for each dataset
train_intensity <- anger_res$train_intensity
dev_intensity <- anger_res$dev_intensity 
test_intensity <- anger_res$test_intensity 


# -----------------------------------------------------------------------------
# Fit lasso model on DTM
# -----------------------------------------------------------------------------

# Register parallel backend (see glment help for info)
library(doMC)
registerDoMC(cores=4)

# Lasso needs a matrix. Convert DTM to matrix 
train_mat <- as.matrix(train_dtm)

# Cross validation of lambda value for lasso 

fit_anger <- 
	cv.glmnet(
		x=train_mat,     # Text features/ 
		y=train_intensity,      # Emotional intensity
		alpha = 1,
		type.measure = "mse", 
		parallel=TRUE)

# -----------------------------------------------------------------------------
# Prediction
# -----------------------------------------------------------------------------
mse <- function(y_true, y_pred){

	if(length(y_true) != length(y_pred)){
		stop("Length of the input vectors does not match")
	}

	sum((y_true-y_pred)^2)/length(y_pred)
}

# Test prediction on dev DTM
dev_mat <- as.matrix(dev_dtm)

anger_pred <- predict(fit_anger, s=fit_anger$lambda.1se, newx=dev_mat)

mse(dev_intensity, anger_pred)

# Repeat for test data
test_mat <- as.matrix(test_dtm)

anger_pred_test <- predict(fit_anger, s=fit_anger$lambda.1se, newx=test_mat)

mse(test_intensity, anger_pred_test)


plot(anger_pred_test, test_intensity)



# RESULT : all predictions = 0.5013




