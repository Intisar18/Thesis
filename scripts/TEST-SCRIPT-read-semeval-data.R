library(purrr) 
library(dplyr)
library(readr)

# File paths are relative to the top-level folder 

data_dir <- 'semeval-data'

# Get contest names which can be used
task_dir <- dir(data_dir)

data_files <- list.files(data_dir, recursive = TRUE, full.names = TRUE)

# To read in a different set of data the "EI-reg" 
# argument can be changed to "EI-oc" or "E-c", etc. 


# # Function to read in data from directory for each task
# read_task_files <- function(task_name, data_files) {

#   is_match <- grepl(task_name, data_files, fixed = TRUE)
  
#   match_files <- data_files[is_match]
  
#   match_data <- purrr::map(match_files, read.delim) 
  
#   return(match_data)
  
# }

# Each task name is the name of the directory 
message(task_dir[1])

# Read each datasets for each task
ec_data <- read_task_files(task_dir[1], data_files) 

eioc_data <- read_task_files(task_dir[2], data_files) 

eireg_data <- read_task_files(task_dir[3], data_files) 

valenceoc_data <- read_task_files(task_dir[4], data_files) 

valencereg_data <- read_task_files(task_dir[5], data_files) 


# Make list of the datasets 
all_datasets <- list(ec_data, eioc_data, eireg_data, valenceoc_data, valencereg_data)

# The names of the datasets match the task folders 
names(all_datasets) <- task_dir


# Print out dimensions for each dataset in order 
# 1. dev
# 2. test 
# 3. train
modify_depth(all_datasets, 2, dim) 

