# SIF sentence model 

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
library(broom)
library(dplyr)
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

	dat
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

registerDoParallel(4)
getDoParWorkers()

# -----------------------------------------------------------------------------
# Remove near zero variance predictors
# -----------------------------------------------------------------------------



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

model_list <- 
  caretList(
    y~., 
    data=train_xy$anger, 
    trControl=anger_control, 
    methodList = c("lasso","rf", "xgbTree")
    )


# -----------------------------------------------------------------------------
# Model correlation
# -----------------------------------------------------------------------------



# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Greedy ensemble
greedy_ensemble <- caretEnsemble(model_list)
modelCor(resamples(model_list))
summary(greedy_ensemble)


model_preds <- lapply(model_list, predict, newdata=dev_xy$anger$data)
model_preds <- data.frame(model_preds)
ens_preds <- predict(greedy_ensemble, newdata=dev_xy$anger$data)
ens_preds <- data.frame(ens_preds)


model_res <- apply(model_preds, 2, function(x) cor(x, dev_xy$anger$data$y))
ens_res <- apply(ens_preds, 2, function(x) cor(x, dev_xy$anger$data$y))


baseline_res <- as_tibble(t(data.frame(c(model_res, ens_res))))

saveRDS(baseline_res, file.path("results", "baseline_caret.RDS"))



# -----------------------------------------------------------------------------
# Stacking
# -----------------------------------------------------------------------------


glm_ensemble <- caretStack(
  model_list,
  method="ridge",
  trControl=trainControl(
    method="cv",
    number=10,
    savePredictions="final"
  )
)

stack_ens_preds <- predict(glm_ensemble, newdata=dev_xy$anger$data)
stack_ens_preds <- data.frame(stack_ens_preds)


apply(model_preds, 2, function(x) cor(x, dev_xy$anger$data$y))
apply(stack_ens_preds, 2, function(x) cor(x, dev_xy$anger$data$y))



