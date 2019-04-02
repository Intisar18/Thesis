# Return training data for all emotions in single data set
get_combined_training_df <- function(){

	# Updated script to read in data
	emotions <- c("anger", "joy", "sadness", "fear")

	# Read in all the datasets
	all_data <- lapply(emotions, get_data_set)

	# Get training sets
	training_sets <- purrr::map(all_data, ~.x$train)

	train_df <- do.call(rbind, training_sets)

	# Create single data.frame of all data

	names(train_df) <- c("ID", "Tweet", "Affect", "Intensity")

	train_df
}
