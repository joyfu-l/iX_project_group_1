library(caret)
library(skimr)
library(RANN)
library(tidyverse)

df <- read.csv("data/raw/teaching_training_data.csv")
view(df)


set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- heart[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- heart[-trainRowNumbers,]
