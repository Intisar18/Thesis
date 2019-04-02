library(purrr)
library(dplyr)
library(readr)
library(ggplot2)
library(tidytext)
# Updated script to read in data

# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Tidy
# -----------------------------------------------------------------------------
# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)


# All training data with updated column names
train_df <- get_combined_training_df()

# Get cleaned word counts of trainind data
word_count <- get_training_word_count(train_df)

# cutoff <- 20

p1 <-
word_count %>%
	dplyr::filter(nn < cutoff) %>%
	ggplot(aes(nn)) +
	geom_histogram(bins = 30) +
	theme_light()


p2 <-
word_count %>%
	dplyr::filter(nn > cutoff) %>%
	ggplot(aes(nn)) +
	geom_histogram(bins=100) +
	xlim(0, 4000) +
	theme_light()


# -----------------------------------------------------------------------------
# Join plots
# -----------------------------------------------------------------------------
library(patchwork)
p1 + p2

# -----------------------------------------------------------------------------
# Save data
# -----------------------------------------------------------------------------

save(tidy_df, file=file.path("data", "tidy_df.RData"))
