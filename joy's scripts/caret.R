library(caret)
library(skimr)
library(RANN)
library(tidyverse)
library(cluster)
library(lubridate)

df <- read.csv("data/raw/teaching_training_data.csv")
view(df)

df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

df_cluster <- df %>% 
  filter(!is.na(working) & !is.na(age) & !is.na(peoplelive))

df_cluster <- df_cluster %>% 
  select(X, unid, working, age, peoplelive)

view(df_cluster)


set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$working, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- heart[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- heart[-trainRowNumbers,]
