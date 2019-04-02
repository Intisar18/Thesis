# -----------------------------------------------------------------------------
# Baseline model using careEnsemble
# -----------------------------------------------------------------------------

library(broom)
library(dplyr)
library(purrr)
library(glmnet)
library(magrittr)
library(randomForest)
library(tibble)


# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")

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

	# 
	to_remove <- c("n_commas", "n_digits", "n_periods", "n_caps", 
		           "n_tobe")

	# Manually remove near zero variance predictors
	combined_features <- dplyr::select(combined_features, -to_remove)
	
	combined_features <- dplyr::select(combined_features, -n_urls)
	
	# Remove word emebddings
	combined_features <- dplyr::select(combined_features, -starts_with("V")) 
	combined_features <- dplyr::select(combined_features, -starts_with("w")) 
	
	# Remove id
	combined_features <- dplyr::select(combined_features, -id)

	

	y <- dat$data[[emotion]][["Intensity Score"]]	

	# Changed to add the response y to the dataset
	combined_features$y = y
	list(data = combined_features)
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
# Prepare ensemble
# -----------------------------------------------------------------------------

library(caret)
library(caretEnsemble)
library(mlbench)
library(pROC)
# library(caTools)
library(doParallel) 

# -----------------------------------------------------------------------------
# Set up for parallel processing
# -----------------------------------------------------------------------------

registerDoParallel(4)
getDoParWorkers()




# anger_data <- preProcess(train_xy$anger$data, method = "nzv")

# -----------------------------------------------------------------------------
# Training parameters
# -----------------------------------------------------------------------------
anger_control <- trainControl(
  method="cv",
  number=5,
  savePredictions="final",
  summaryFunction=defaultSummary, 
  allowParallel=TRUE	
  )

# -----------------------------------------------------------------------------
# List of models to run
# -----------------------------------------------------------------------------
system.time({ 

model_lists <- 
  caretList(
    y~., 
    data=train_xy$anger$data, 
    trControl=anger_control, 
    methodList = c("lasso","rf", "xgbTree")
    )

})

# -----------------------------------------------------------------------------
# Model correlation
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Greedy ensemble
greedy_ensemble <- caretEnsemble(model_list)
modelCor(resamples(model_list))
summary(greedy_ensemble)


# -----------------------------------------------------------------------------
# Check location
# -----------------------------------------------------------------------------	
## Run anger model

map(emotions, ~predict(, newdata=dev_xy$anger$data))

stack_ens_preds <- predict(glm_ensemble, newdata=dev_xy$anger$data)
stack_ens_preds <- data.frame(stack_ens_preds)


apply(model_preds, 2, function(x) cor(x, dev_xy$anger$data$y))
apply(stack_ens_preds, 2, function(x) cor(x, dev_xy$anger$data$y))

# apply(model_preds, 2, function(x) cor(x, test_xy$anger$data$y))
# apply(stack_ens_preds, 2, function(x) cor(x, test_xy$anger$data$y))




model_preds <- lapply(model_list, predict, newdata=dev_xy$anger$data)
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=dev_xy$anger$data)
ens_preds <- data.frame(ens_preds)


model_res <- apply(model_preds, 2, function(x) cor(x, dev_xy$anger$data$y))
ens_res <- apply(ens_preds, 2, function(x) cor(x, dev_xy$anger$data$y))


baseline_res <- as_tibble(t(data.frame(c(model_res, ens_res))))

saveRDS(baseline_res, file.path("results", "baseline_caret.RDS"))

# -----------------------------------------------------------------------------

stack_ensemble <- function(model_list){
	glm_ensemble <- caretStack(
	  list,
	  method="ridge",
	  trControl=trainControl(
	    method="cv",
	    number=10,
	    savePredictions="final"
	  )
	)
}

# Run all ensembles 


# Name ensembles
names(stack_ensemble) <- emotions


