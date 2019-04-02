library(purrr)
library(dplyr)
library(readr)
library(ggplot2)

# for rare model
library(rare)
library(Matrix)

# Script prepares training and dev data to be fit using "rare" model


# Load functions from the R file
R_files <- file.path("R", list.files("R"))
purrr::map(R_files, source)

# -----------------------------------------------------------------------------
# Load training and dev data
# -----------------------------------------------------------------------------
all_data <- get_all_data() 

word_features <- readRDS(file.path("data", "word_features.RDS"))
res <- prepare_train_dev_rare_model_data("anger", all_data, word_features) 

train_dtm <- res$train_dtm 
dev_dtm <- res$dev_dtm 

train_intensity <- res$train_intensity
dev_intensity <- res$dev_intensity 

# -----------------------------------------------------------------------------
# Fit Model
# -----------------------------------------------------------------------------

# Load cluster data
word_clust <- readRDS(file.path("data", "word_clust.RDS"))


# Vignette : https://cran.r-project.org/web/packages/rare/vignettes/rare-vignette.html

# The default settings
# rarefit(y, X, A = NULL, Q = NULL, hc, intercept = T, lambda = NULL,
# 	alpha = NULL, nlam = 50, lam.min.ratio = 1e-04, nalpha = 10,
# 	rho = 0.01, eps1 = 1e-06, eps2 = 1e-05, maxite = 1e+06)

# Fit the rare model with the default parameters

# -----------------------------------------------------------------------------
# RUN FOR NEW FIT
# -----------------------------------------------------------------------------

# ourfit <-
# rarefit(
# 	y = train_intensity,
# 	X = train_dtm,
# 	hc = word_clust,
# 	nlam = 20,  			# Reduced to 20
# 	lam.min.ratio = 1e-06,  # Decreased
# 	nalpha = 10,			# Default
# 	rho = 0.01, 			# Default
# 	eps1 = 1e-05, 			# Increased
# 	eps2 = 1e-05, 			# Default
# 	maxite = 1e+04)			# Decreased

# (Running time ~ 2 hrs)


# -----------------------------------------------------------------------------
# Parameter tuning
# -----------------------------------------------------------------------------

ourfit <- readRDS(file.path("data", "ourfit1.RDS"))

# Example: 
# Cross validation
ourfit_cv <- 
	rarefit.cv_test(ourfit,
		y = train_intensity,
		X = train_dtm,
		nfolds = 5, 
		rho = 0.01, 
        eps1 = 1e-5, 
        eps2 = 1e-5, 
        maxite = 1e4)

# (Running time - 40 mins?) Failed

# -----------------------------------------------------------------------------
# Prediction
# -----------------------------------------------------------------------------

# Example: # Prediction on test set
pred <- rarefit.predict_test(ourfit, ourfit_cv, dev_dtm)
pred.error <- mean((pred - dev_intensity)^2)
pred.error
