# Updated script to read in data 

emotions <- c("anger", "joy", "sadness", "fear")


# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# Load and tidy anger data 
anger_data <- get_data_set("anger")

anger_train <- anger_data$train

tidy_anger <- tidy_tweet_data(anger_train)

