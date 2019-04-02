# -----------------------------------------------------------------------------
# Feature set # 1: Load results 
# -----------------------------------------------------------------------------
library(broom)
library(dplyr)
library(purrr)
library(glmnet)
library(magrittr)
library(randomForest)
library(tibble)


# -----------------------------------------------------------------------------
# Original data
# -----------------------------------------------------------------------------

# Updated script to read in data 
emotions <- c("anger", "joy", "sadness", "fear")

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# Read in all the datasets 
# all_data <- lapply(emotions, get_data_set) 

# Get test data with intensity score
test_sets <- lapply(emotions, 
				   get_data_set, 
				   data_dir = 'semeval-test-data', 
				   data_names = 'test') 

test_sets <- unlist(test_sets, recursive = FALSE)


# Name the data sets y the emotions
names(test_sets) <- emotions

# Get training sets
# train_sets <- purrr::map(all_data, ~.x$train )
# dev_sets   <- purrr::map(all_data, ~.x$dev )
# test_sets   <- purrr::map(all_data, ~.x$test) 
# -----------------------------------------------------------------------------
# Load baseline features
# -----------------------------------------------------------------------------

basic_features_test <- readRDS(file.path("data", "basic_features_test.RDS"))
nrc_features_test <- readRDS(file.path("data", "nrc_features_test.RDS"))


# -----------------------------------------------------------------------------
# Join all data to make retrieving data from easier
# -----------------------------------------------------------------------------
test_data <- 
	list(data = test_sets, 
		basic = basic_features_test, 
		nrc = nrc_features_test)


# -----------------------------------------------------------------------------
# Fo4mat data for prediction
# -----------------------------------------------------------------------------

prep_baseline_features <- function(emotion){

	dat = test_data

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

test_xy <- map(emotions,  prep_baseline_features)

names(test_xy) <- emotions 

# -----------------------------------------------------------------------------
# Caret models
# -----------------------------------------------------------------------------

aws_baseline_list_path <- 
	file.path("results", "aws-results", "baseline_model_lists.RDS") 


baseline_model_list <- 
	readRDS(aws_baseline_list_path) 


baseline_ensembles_path <- 
		file.path("results", "aws-results", "baseline_ensembles.RDS") 


baseline_ensembles <- 
	readRDS(baseline_ensembles_path) 

# -----------------------------------------------------------------------------
# Prediction
# -----------------------------------------------------------------------------

baseline_pred <- 
	map(emotions, 
		~predict(baseline_model_list[[.x]], 
			newdata=test_xy[[.x]]$data))

names(baseline_pred) <- emotions 

# pred_ens <- 
# 	predict(baseline_ensembles$anger$ens_model, newdata=test_xy$anger$data))

get_cor <- function(x, emotion){
	cor(x, test_xy[[emotion]]$data$y)
}

baseline_cor <- map(emotions, 
	~apply(baseline_pred[[.x]], 2, get_cor, emotion=.x))


baseline_cor_tb <- 
	do.call(rbind, baseline_cor) %>% 
	as_tibble

baseline_cor_tb$emotion <- emotions 



# -----------------------------------------------------------------------------
# Ensemble
# -----------------------------------------------------------------------------
library(caretEnsemble)
library(caret) 

ridge_ensemble <- function(emotion){
	caretStack(
	  baseline_model_list[[emotion]],
	  method="ridge",
	  trControl=trainControl(
	    method="cv",
	    number=10,
	    savePredictions="final"
	  )
	)
}

# Prediciton
ens_stack <- map(emotions, ridge_ensemble)

names(ens_stack) <- emotions 

ens_pred <- map(emotions, 
	~predict(ens_stack[[.x]], newdata=test_xy[[.x]]$data))

names(ens_pred) <- emotions 

ens_cor <- map(emotions, 
	~cor(ens_pred[[.x]], test_xy[[.x]]$data$y))

ens_cor

ens_cor <- unlist(ens_cor)

baseline_cor_tb$stack <- ens_cor

saveRDS(baseline_cor_tb, file.path("results", "baseline_cor_tb.RDS"))


