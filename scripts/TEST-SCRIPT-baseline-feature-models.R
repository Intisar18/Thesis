# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------
libray(dplyr)

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

# -----------------------------------------------------------------------------
# Load baseline features
# -----------------------------------------------------------------------------


basic_features <- readRDS(file.path("data", "basic_features.RDS"))
nrc_features <- readRDS(file.path("data", "nrc_features.RDS"))


# -----------------------------------------------------------------------------
# Prepare data
# -----------------------------------------------------------------------------

anger_basic <- basic_features$anger 
anger_nrc <- nrc_features$anger

# Counts and embedding
anger_features1 <- cbind(anger_basic, anger_nrc)
anger_train <- training_sets$anger 
y <- anger_train$`Intensity Score`

rf_res0 <- randomForest(x=anger_features0,  y = y)

# -----------------------------------------------------------------------------
# Add columns with features normalized by word count
# -----------------------------------------------------------------------------
with(anger_basic,
	plot(n_puncts, sent_bing)
)


anger_freq <- 
anger_basic %>% 
	dplyr::select( starts_with("n_") ) %>% 
	mutate_all(funs(./n_words))


names(anger_freq) <- paste0("freq_", names(anger_freq))
anger_freq$sent_afinn <- anger_basic$sent_afinn
anger_freq$sent_bing <- anger_basic$sent_bing



# -----------------------------------------------------------------------------
# 
# -----------------------------------------------------------------------------

dim(anger_basic)
dim(anger_nrc)

anger_no_embed <- anger_basic[1:29]

# -----------------------------------------------------------------------------
# Create basic random forest model
# -----------------------------------------------------------------------------

library(randomForest)


# Frequency instead of counts, no embedding
anger_features0 <- cbind(anger_freq, anger_nrc)

# Counts and embedding
anger_features1 <- cbind(anger_basic, anger_nrc)

# Counts and no embedding
anger_features2 <- cbind(anger_no_embed, anger_nrc)


anger_train <- training_sets$anger 

y <- anger_train$`Intensity Score`




# -----------------------------------------------------------------------------
# Model 2 with word embeddings
# -----------------------------------------------------------------------------

rf_res0 <- randomForest(x=anger_features0,  y = y)
rf_res0 

varImpPlot(rf_res0)

predict1 <- predict(rf_res0, anger_features0)

xx = seq(0,1, by = 0.1)
yy = xx 
df <- data.frame(xx, yy)

par(mfrow= c(2,1))
mod <- lm(yy ~ xx, data = df)

plot(predict1, y, col = "slateblue3")
abline(mod)


plot(y, predict1 - y, col = "slateblue3")

# -----------------------------------------------------------------------------
# Model 1 with word embeddings
# -----------------------------------------------------------------------------

rf_res1 <- randomForest(x=anger_features1,  y = y)
rf_res1 

varImpPlot(rf_res1)

predict1 <- predict(rf_res1, anger_features1)

xx = seq(0,1, by = 0.1)
yy = xx 
df <- data.frame(xx, yy)

par(mfrow= c(2,1))
mod <- lm(yy ~ xx, data = df)

plot(predict1, y, col = "slateblue3")
abline(mod)


mse1 = sum((predict1 - y)^2)/length(y)

plot(y, predict1 - y, col = "slateblue3")


# -----------------------------------------------------------------------------
# Model 2 with word embeddings
# -----------------------------------------------------------------------------

rf_res2 <- randomForest(x=anger_features2,  y = y)
rf_res2 

varImpPlot(rf_res2)

predict2 <- predict(rf_res2, anger_features2)

mse2 = sum((predict2 - y)^2)/length(y)

plot(predict2, y, col = "slateblue3")

# -----------------------------------------------------------------------------
# NOTES: Best results was with anger_features2 : Counts and no embedding
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Create plot
# -----------------------------------------------------------------------------

# Save
# jpeg(file.path("images", "baseline-model.jpg"), width = 800, height = 500)

par(mfrow = c(1, 2))

plot(predict2, y, col = "slateblue3", xlab="predicted", ylab="actual", 
		main="Intensity vs. predicted intensity")


varImpPlot(rf_res2)

# dev.off()






