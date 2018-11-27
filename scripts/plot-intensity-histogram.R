library(purrr) 
library(dplyr)
library(readr)
library(ggplot2) 

# Updated script to read in data 

emotions <- c("anger", "joy", "sadness", "fear")

pal <- c("firebrick2", "orchid3", "steelblue3", "goldenrod3")

names(pal) <- emotions

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# Read in all the datasets 
all_data <- lapply(emotions, get_data_set) 

# Name the data sets y the emotions
names(all_data) <- emotions 

# Get training sets
training_sets <- purrr::map(all_data, ~.x$train )

# Combine all training sets for plotting 
combined_data <- do.call(rbind, training_sets)

names(combined_data) <- c("ID", "Tweet", "Affect", "Intensity")

# Plot histograms
combined_data %>%
	ggplot(aes(Intensity, fill = Affect)) + 
	geom_histogram(bins = 20, size = 0.5, color = "white") +
	scale_fill_manual(values = pal) + 
	facet_wrap(~Affect) +  
	theme_minimal()  
