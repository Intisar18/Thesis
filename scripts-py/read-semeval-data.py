#------------------------------------------------------------------------------
# This script: 
# 	1. Loads the EI-reg data
#   2. Gets the Sentiment-Neuron features for each tweet
#   3. Saves the output as .txt file
#
# This script depends on the repository: 
# https://github.com/openai/generating-reviews-discovering-sentiment
#------------------------------------------------------------------------------

# Libraries
import glob 
import os 
import pandas as pd 

#------------------------------------------------------------------------------
# This section sets up the openAI model
#------------------------------------------------------------------------------
# Sentiment 
from encoder import Model 

# Set up sentiment model 
model = Model() 

# Test feature extraction
demo_text = ['text']
text_features = model.transform(demo_text)

#------------------------------------------------------------------------------
# Load data
#------------------------------------------------------------------------------

# NOTE: This is folder is is not included in the repository. 
path = '/generating-reviews-discovering-sentiment'
os.chdir(path)


# Path to data. Working directory must be in the thesis folder
# Folder contains "EI-reg" data
data_path= "/semeval-data/EI-reg"


# Create empty variables used in for loops
filepaths = []
data = [] 
test_data = [] 
features = [] 
savepaths = [] 

# Create list of file paths
for path in os.listdir(data_path): 
	print(data_path + "/" + path)
	filepaths.append(data_path + "/" + path)


# Check 
filepaths[1]


# Create list of data files for each file in path
for path in filepaths:
	data.append(pd.read_csv(path, sep='\t'))


#------------------------------------------------------------------------------
# Make new file names and get features
#------------------------------------------------------------------------------

# Make new paths 
for path in filepaths:
	savepath = path.replace('semeval-data/EI-reg', 'sentiment-neuron-data')
	savepaths.append(savepath) 


# Get features for all data
for df in data: 
	tweets = list(df['Tweet'])
	sent_feature = model.transform(tweets)
	features.append(sent_feature)

# Save results 
for feature, path in zip(features, savepaths):
	pd.DataFrame(feature).to_csv(path)

