# SIF sentence model 

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
library(broom)
library(dplyr)
library(purrr)
library(glmnet)
library(magrittr)
library(randomForest)
library(tibble)

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Load SIF sentence data
# -----------------------------------------------------------------------------

sif_train <- readRDS(file.path("data", "sentence_sif_df.RDS"))
sif_test <- readRDS(file.path("data", "sentence_sif_df_test.RDS"))

names(sif_train) <- emotions 
names(sif_test)  <- emotions

all_data <- list(train =sif_train, test=sif_test)

# -----------------------------------------------------------------------------
# Prepare data for being fit 
# -----------------------------------------------------------------------------

separate_xy <- function(emotion, dataset){

	dat = all_data[[dataset]][[emotion]]

	y <- dplyr::pull(dat, `Intensity Score`)

	x <- dplyr::select(dat, -ID, -`Intensity Score`)

	list(x = x, y = y)
}


# -----------------------------------------------------------------------------
# Get data
# -----------------------------------------------------------------------------
train_xy <- map(emotions, ~separate_xy(.x, dataset = "train"))

# dev_xy <- lapply(emotions, prep_baseline_features, dataset="dev")
test_xy <- map(emotions, ~separate_xy(.x, dataset="test"))



names(train_xy) <- emotions 
names(test_xy) <- emotions 
# names(dev_xy) <- emotions 


# -----------------------------------------------------------------------------
# Fit models
# -----------------------------------------------------------------------------
rf_fits1 <- map(emotions, 
	~randomForest(train_xy[[.x]]$x, c(train_xy[[.x]]$y)), 
		ntree=500, mtry = 100) 

rf_fits2 <- map(emotions, 
	~randomForest(train_xy[[.x]]$x, train_xy[[.x]]$y), 
		ntree=1000, mtry = 50) 


names(rf_fits1) <- emotions
names(rf_fits2) <- emotions

# -----------------------------------------------------------------------------
# Predict
# -----------------------------------------------------------------------------

mse <- function(y_true, y_pred){

	if(length(y_true) != length(y_pred)){
		stop("Length of the input vectors does not match")
	}

	sum((y_true-y_pred)^2)/length(y_pred)
}


predict_rf_emotion <- function(emotion, xy, fit){

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
test_pred1 <- 
	map(emotions, 
		~predict_rf_emotion(.x, xy = test_xy, fit = rf_fits1[[.x]]))

# Get random forest prediction
test_pred2 <- 
	map(emotions, 
		~predict_rf_emotion(.x, xy = test_xy, fit = rf_fits2[[.x]]))

# test_pred <- map(test_pred, unlist)

# Add emotion name to lists
names(test_pred1) <- emotions 
names(test_pred2) <- emotions 

# Get MSE for each emotion
mse1 <- map(emotions, ~mse_rf_emotion(.x, test_pred1, test_xy))
mse2 <- map(emotions, ~mse_rf_emotion(.x, test_pred2, test_xy))


names(mse1) <- emotions 
names(mse2) <- emotions

# Compare difference
mse1 
mse2 

# -----------------------------------------------------------------------------
# There is not much difference between the two test predictions
# -----------------------------------------------------------------------------


# Calculate MSE
test_mse <- 
	lapply(emotions, mse_rf_emotion, pred=test_pred, xy=test_xy)
names(test_mse) <- emotions


# Rename
test_mse_rf_sif_sentence <- mse1
# test_mse_rf_baseline <- test_mse 

saveRDS(test_mse_rf_sif_sentence, 
	file.path("results", "test_mse_rf_sif_sentence.RDS")) 


