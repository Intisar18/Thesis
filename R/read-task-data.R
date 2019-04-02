# Functions for semeval-2018 data 

# Function to read in data from directory for each task
read_task_files <- function(task_name, data_files) {

  is_match <- grepl(task_name, data_files, fixed = TRUE)
  
  match_files <- data_files[is_match]
  
  match_data <- purrr::map(match_files, readr::read_tsv) 
  
  return(match_data)
  
}


# Get data for one emotion
# (Code moved from "read-semeval-data.R" script)
get_data_set <- function(
	emotion, 
	data_dir = "semeval-data",
	data_names = c("dev", "test", "train"))
{

	R_files <- file.path("R", list.files("R"))

	purrr::map(R_files, source)


	data_files <- get_data_file_names(data_dir)

	# Make file path name 
	folder_name <- paste0("EI-reg-En-", emotion)
	
	# Read in anger only motional intensity regression data
	ei_reg_data <- read_task_files(folder_name, data_files)

	# Check data dimensions 
	# purrr::map(ei_data, dim)

	# Data is in order (dev, test, train)
	names(ei_reg_data) <- data_names

	return(ei_reg_data)
}



# Function to get relative path to semeval-2018 data
get_data_file_names <- function(data_dir = "semeval-data"){
	
	# Get contest names which can be used
	task_dir <- dir(data_dir)

	data_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)

	if(length(data_files) < 1) {
		stop(paste0("Make sure that the current working directory is", 
			" set to top folder of the 'Affect-in-tweets' project"))
	}
	
	data_files 
}