get_all_data <- function(){
	
	# Updated script to read in data
	emotions <- c("anger", "joy", "sadness", "fear")

	# Load functions from the R file
	R_files <- file.path("R", list.files("R"))
	purrr::map(R_files, source)

	# Read in all the datasets
	all_data <- lapply(emotions, get_data_set)

	# Name the data sets y the emotions
	names(all_data) <- emotions	

	all_data
}
