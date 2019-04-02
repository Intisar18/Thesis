# 

# Calculate word probabilities (on combined corpus?) 

# 1. Create master list of words that have word-vectors (see rare-model script) 
# 2. Calculate word probabilities using some tidy method 
# 3. Save dataframe with word + p(w) + word(vector) ? 

# Assume we have something like that created in (3) 
# Assume we have the tidy format for the tweets: word + count + tweet ID 

# Calculate v_s

# 1. Join word probability and word vector to tidy formatted dictionary 
# 2. Calculate weight column: 

# a/(a + p(w))
# 3. calculate weighted vector column 
# ( wt * vector * count)
# 4. Group by tweet ID and sum = V_S dataframe.

# 5. as.matrix(V_S data frame)
# 6. PCA -> 1st component = c_0 
# 7. V_S <- Matrix - c_0 (recycling) 

# 8. Convert back to data.frame (with tweet IDs)

# Return data.frame with ID | Tweet vector



