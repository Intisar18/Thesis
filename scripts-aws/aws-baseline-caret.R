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


# aws: setwd("project") 

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
  
  # Remove features with near zero variance
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
  
  
  y <- dat$data[[emotion]]$`Intensity Score`	
  
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
library(parallel)

# -----------------------------------------------------------------------------
# Set up for parallel processing
# -----------------------------------------------------------------------------
# registerDoParallel(10)
#getDoParWorkers()

cluster <- makeCluster(detectCores() - 5) # convention to leave 1 core for OS
registerDoParallel(cluster)

# -----------------------------------------------------------------------------
# All emotion fits
# -----------------------------------------------------------------------------

model_control <- 
  trainControl(
    method="cv",
    number=5,
    savePredictions="final",
    summaryFunction=defaultSummary, 
    allowParallel=TRUE	
  )


fit_caret_models <- function(emotion) {

  message("Training models for emotion ", emotion) 
  
  model_lists <- 
    caretList(
      y~., 
      data=train_xy[[emotion]]$data, 
      trControl= model_control, 
      methodList = c("lasso","rf", "xgbTree")
    )
}

# -----------------------------------------------------------------------------
# Run model
# -----------------------------------------------------------------------------

# Run cross-validated models 
# model_lists <- map(emotions, fit_caret_models)

system.time({
  anger_res <- map("anger", fit_caret_models)
})

saveRDS(anger_res, file.path("results", "baseline_anger.RDS"))

# anger_res <- map("joy", fit_caret_models)

system.time({
  sadness_res <- map("sadness", fit_caret_models)
})

saveRDS(sadness_res, file.path("results", "baseline_sadness.RDS"))

system.time({
  fear_res <- map("fear", fit_caret_models)
})

saveRDS(fear_res, file.path("results", "baseline_fear.RDS"))


system.time({
  joy_res <- map("joy", fit_caret_models)
})


saveRDS(joy_res, file.path("results", "baseline_joy.RDS"))


#--------------------------------------------------------------------
# Stop Cluster
#--------------------------------------------------------------------

stopCluster(cluster)


#--------------------------------------------------------------------
# Stop Cluster
#--------------------------------------------------------------------

model_lists <- list(
  anger_res[[1]],  
  joy_res[[1]],
    sadness_res[[1]],
    fear_res[[1]] 
    )

# Name results
names(model_lists) <- emotions 

# Model correlation
baseline_model_correlations <- map(model_lists, ~modelCor(resamples(.x)))

# Run greedy ensemble
baseline_ensembles <- map(model_lists, caretEnsemble)

# Model summaries 
map(baseline_ensembles, summary)


baseline_stacked <- map(model_lists, 
                        ~caretStack(.x, 
                                    method="ridge", 
                                    trControl=trainControl(
                                      method="cv",
                                      number=10,
                                      savePredictions="final"
                                    )))

#--------------------------------------------------------------------
# Format results
#--------------------------------------------------------------------


baseline_ensembles

baseline_ensembles_error_table <- 
  map(baseline_ensembles, ~.x$error) %>% 
  do.call(rbind,.) %>% 
  as_tibble(.)


baseline_stacked_error_table <- 
  map(baseline_stacked, ~.x$error) %>% 
  do.call(rbind,.) %>% 
  as_tibble(.)

baseline_ensembles_error_table 
baseline_stacked_error_table 


# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------

saveRDS(model_lists, file.path("results", "baseline_model_lists.RDS"))
saveRDS(baseline_model_correlations, file.path("results", "baseline_model_corelations.RDS"))
saveRDS(baseline_ensembles, file.path("results", "baseline_ensembles.RDS"))
saveRDS(baseline_stacked, file.path("results", "baseline_stacked.RDS"))
saveRDS(baseline_ensembles_error_table, file.path("results", "baseline_ensembles_error_table.RDS"))
saveRDS(baseline_stacked_error_table, file.path("results", "baseline_stacked_error_table.RDS"))

# -----------------------------------------------------------------------------
# Prediction and Pearson's correlation
# -----------------------------------------------------------------------------

model_predictions <- function(emotion){
  
  model_preds <- lapply(model_list, predict, newdata=dev_xy[[emotion]]$data)
  model_preds <- data.frame(model_preds)
  
  ens_preds <- predict(greedy_ensemble, newdata=dev_xy[[emotion]]$data)
  ens_preds <- data.frame(ens_preds)
  
  model_cor <- 
    apply(model_preds, 2, function(x) cor(x, dev_xy[[emotion]]$data$y))
  ens_cor <- 
    apply(ens_preds, 2, function(x) cor(x, dev_xy[[emotion]]$data$y))
  
  baseline_cor <- as_tibble(t(data.frame(c(model_cor, ens_cor))))	
}



