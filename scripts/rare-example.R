library(rare)
library(Matrix)

# Design matrix = document-term matrix
dim(data.dtm)

#> [1] 500 200
# Ratings for the reviews in data.dtm
length(data.rating)

#> [1] 500
# The data set contains 200 adjectives in the sample and most of them are highly sparse. Below is a histogram of percentage of reviews using adjective.

hist(colMeans(
	sign(data.dtm)) * 100, 
	 breaks = 50,
	 main = "Histogram of Adjective Rarity in the TripAdvisor Sample", 
     xlab = "% of Reviews Using Adjective")


set.seed(100)
ts <- sample(1:length(data.rating), 400) # Train set indices

ourfit <- rarefit(y = data.rating[ts], X = data.dtm[ts, ], 
	hc = data.hc, 
	lam.min.ratio = 1e-6, 
	nlam = 20, 
	nalpha = 10, 
	rho = 0.01, 
	eps1 = 1e-5, 
	eps2 = 1e-5, 
	maxite = 1e4)

# Withou our data: 

# Cross validation
ourfit.cv <- rarefit.cv(ourfit, y = data.rating[ts], X = data.dtm[ts, ],
                        rho = 0.01, eps1 = 1e-5, eps2 = 1e-5, maxite = 1e4)

# Prediction on test set
pred <- rarefit.predict(ourfit, ourfit.cv, data.dtm[-ts, ])
pred.error <- mean((pred - data.rating[-ts])^2)
pred.error