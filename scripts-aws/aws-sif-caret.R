# SIF sentence model 

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
library(broom)
library(purrr)
library(dplyr)
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

prep_xy <- function(emotion, dataset){
  
  dat = all_data[[dataset]][[emotion]]
  dat <- 
    dat %>% 
    dplyr::rename(y = `Intensity Score`)
  
  dat <- dplyr::select(dat, -ID)
  
  list(data = dat)
}

# -----------------------------------------------------------------------------
# Get data
# -----------------------------------------------------------------------------
train_xy <- map(emotions, ~prep_xy(.x, dataset = "train"))
test_xy <- map(emotions, ~prep_xy(.x, dataset="test"))

names(train_xy) <- emotions 
names(test_xy) <- emotions 
# names(dev_xy) <- emotions 

# -----------------------------------------------------------------------------
# Fit care ensemble
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

#registerDoParallel(10)
#getDoParWorkers()

cluster <- makeCluster(detectCores() - 5) # convention to leave 1 core for OS
registerDoParallel(cluster)


# -----------------------------------------------------------------------------
# Training parameters
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
  
  message("Training for emotion: ", emotion)
  model_lists <- 
    caretList(
      y~., 
      data=train_xy[[emotion]]$data, 
      trControl= model_control, 
      methodList = c("lasso","rf", "xgbTree")
    )
}


# -----------------------------------------------------------------------------
# Run cross-validated models 
# -----------------------------------------------------------------------------

system.time({
  anger_res <- map("anger", fit_caret_models)
})

saveRDS(anger_res, file.path("results", "sif_anger.RDS"))

# -----------------------------------------------------------------------------

system.time({
  sadness_res <- map("sadness", fit_caret_models)
})

saveRDS(sadness_res, file.path("results", "sif_sadness.RDS"))

# -----------------------------------------------------------------------------

system.time({
  fear_res <- map("fear", fit_caret_models)
})

saveRDS(fear_res, file.path("results", "sif_fear.RDS"))

# -----------------------------------------------------------------------------

system.time({
  joy_res <- map("joy", fit_caret_models)
})

saveRDS(joy_res, file.path("results", "sif_joy.RDS"))


# -----------------------------------------------------------------------------
# Name and save data
# -----------------------------------------------------------------------------

names(model_lists) <- emotions 

# 
# stopCluster(cluster)
model_lists <- list(
  anger_res[[1]],  
  joy_res[[1]],
  sadness_res[[1]],
  fear_res[[1]] 
)

# -----------------------------------------------------------------------------
# Create Ensembles
# -----------------------------------------------------------------------------

# Name results
names(model_lists) <- emotions 

# Model correlation
sif_model_correlations <- map(model_lists, ~modelCor(resamples(.x)))


# Run greedy ensemble
sif_ensembles <- map(model_lists, caretEnsemble)

# Model summaries 
map(sif_ensembles, summary)


sif_stacked <- map(model_lists, 
                        ~caretStack(.x, 
                                    method="ridge", 
                                    trControl=trainControl(
                                      method="cv",
                                      number=10,
                                      savePredictions="final"
                                    )))

sif_ensembles

sif_ensembles_error_table <- 
  map(sif_ensembles, ~.x$error) %>% 
  do.call(rbind,.) %>% 
  as_tibble(.)


sif_stacked_error_table <- 
  map(sif_stacked, ~.x$error) %>% 
  do.call(rbind,.) %>% 
  as_tibble(.)

sif_ensembles_error_table 
sif_stacked_error_table 



# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------

saveRDS(model_lists, file.path("results", "sif_model_lists.RDS"))
saveRDS(sif_model_correlations, file.path("results", "sif_model_corelations.RDS"))
saveRDS(sif_ensembles, file.path("results", "sif_ensembles.RDS"))
saveRDS(sif_stacked, file.path("results", "sif_stacked.RDS"))
saveRDS(sif_ensembles_error_table, file.path("results", "sif_ensembles_error_table.RDS"))
saveRDS(sif_stacked_error_table, file.path("results", "sif_stacked_error_table.RDS"))

# ----------------------------






