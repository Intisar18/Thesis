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

# -----------------------------------------------------------------------------
# Join Intensity for training data
# -----------------------------------------------------------------------------

train_y <- map(train_sets, ~dplyr::pull(.x, `Intensity Score`))
test_y <- map(test_sets, ~dplyr::pull(.x, `Intensity Score`))
dev_y   <- map(dev_sets,   ~dplyr::pull(.x, `Intensity Score`))

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

# test_data <- map(emotions, ~prep_data(.x, "test"))
# names(test_data) <- emotions 

# -----------------------------------------------------------------------------
# Fit care ensemble
# -----------------------------------------------------------------------------

library(caret)
library(caretEnsemble)
library(mlbench)
library(pROC)
library(doParallel) 

# -----------------------------------------------------------------------------
# Set up for parallel processing
# -----------------------------------------------------------------------------

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
      data=train_data[[emotion]], 
      trControl= model_control, 
      methodList = c("lasso","rf", "xgbTree")
    )
}


# -----------------------------------------------------------------------------
# Run cross-validated models 
# -----------------------------------------------------------------------------

set.seed(2019)

system.time({
  anger_res <- map("anger", fit_caret_models)
})

saveRDS(anger_res, file.path("results", "sentiment_anger.RDS"))

# -----------------------------------------------------------------------------

system.time({
  sadness_res <- map("sadness", fit_caret_models)
})

saveRDS(sadness_res, file.path("results", "sentiment_sadness.RDS"))

# -----------------------------------------------------------------------------

system.time({
  fear_res <- map("fear", fit_caret_models)
})

saveRDS(fear_res, file.path("results", "sentiment_fear.RDS"))

# -----------------------------------------------------------------------------

system.time({
  joy_res <- map("joy", fit_caret_models)
})

saveRDS(joy_res, file.path("results", "sentiment_joy.RDS"))


# -----------------------------------------------------------------------------
# Name and save data
# -----------------------------------------------------------------------------

names(model_lists) <- emotions 

# 
# stopCluster(cluster)
# model_lists <- list(
#   anger_res[[1]],  
#  joy_res[[1]],
#  sadness_res[[1]],
#  fear_res[[1]] 
# )

model_lists <- list(
  sentiment_anger[[1]][[1]], 
  sentiment_joy[[1]][[1]], 
  sentiment_sadness[[1]][[1]],
  sentiment_fear[[1]][[1]]
)

# model_lists <- list(
#  model_lists[[1]][[1]], 
#  model_lists[[2]][[1]], 
#  model_lists[[3]][[1]],
#  model_lists[[4]][[1]]
#)



# -----------------------------------------------------------------------------
# Save result list
# -----------------------------------------------------------------------------

# Name results
names(model_lists) <- emotions 

saveRDS(model_lists, file.path("results", "sentiment_model_lists.RDS"))

# -----------------------------------------------------------------------------
# Create Ensembles
# -----------------------------------------------------------------------------

# Model correlation
sentiment_model_correlations <- map(model_lists, ~modelCor(resamples(.x)))


# Run greedy ensemble
sentiment_ensembles <- map(model_lists, caretEnsemble)

# Model summaries 
map(sentiment_ensembles, summary)


sentiment_stacked <- map(model_lists, 
                   ~caretStack(.x, 
                               method="ridge", 
                               trControl=trainControl(
                                 method="cv",
                                 number=10,
                                 savePredictions="final"
                               )))

sentiment_ensembles

sentiment_ensembles_error_table <- 
  map(sentiment_ensembles, ~.x$error) %>% 
  do.call(rbind,.) %>% 
  as_tibble(.)


sentiment_stacked_error_table <- 
  map(sentiment_stacked, ~.x$error) %>% 
  do.call(rbind,.) %>% 
  as_tibble(.)

sentiment_ensembles_error_table 
sentiment_stacked_error_table 



# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------

saveRDS(sentiment_model_correlations, file.path("results", "sentiment_model_corelations.RDS"))
saveRDS(sentiment_ensembles, file.path("results", "sentiment_ensembles.RDS"))
saveRDS(sentiment_stacked, file.path("results", "sentiment_stacke