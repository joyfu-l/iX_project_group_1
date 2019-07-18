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

df_unid <- df_cluster %>% 
  select(unid)

df_cluster <- df_cluster %>% 
  mutate(num = ifelse(working==TRUE, 1, 0))

df_cluster <- df_cluster %>% 
  select(X, unid, age, peoplelive, num)

df_cluster <- mutate(df_cluster,
                X= factor(X),
                unid = factor(unid))

sapply(df_cluster, class)

view(df_cluster)


dummies_model <- dummyVars(num ~ . , data=df_cluster)

# Create the dummy variables using predict. The Y variable (num) will not be present in heart_mat.
df_cluster_num <- df_cluster$num
df_cluster_mat <- predict(dummies_model, newdata = df_cluster)

preProcess_scale_model <- preProcess(df_cluster[c('age', 'peoplelive')], method=c('center', 'scale'))

df_cluster[c('age', 'peoplelive')] <- predict(preProcess_scale_model, newdata = df_cluster[c('age', 'peoplelive')])

df_cluster$num <- df_cluster_num

set.seed(100)

# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df_cluster$num, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- df_cluster[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- df_cluster[-trainRowNumbers,]


model_rpart <- train(num ~ ., data=trainData, method='rpart')

# Now let's predict for our test data
predicted <- predict(model_rpart, testData[,-length(testData)])

model_rpart






k4 <- kmeans(df_cluster, centers = 4, nstart = 25)

table(k4$cluster)

# look at characteristics within each cluster
k4_cluster <- as.data.frame(k4$cluster)

df_cluster_k4 <- bind_cols(as.data.frame(df_cluster), k4_cluster) %>% 
  rename(cluster = `k4$cluster`)

ggplot(data = df_cluster_k4) + 
  geom_jitter(aes(x = cft_score, y = com_score, color = as.factor(cluster)))


ggplot(data = df_cluster_k4) + 
  geom_jitter(aes(x = cft_score, y = opt_score, color = as.factor(cluster)))

df_cluster_k4_sum <- df_cluster_k4 %>% 
  group_by(cluster) %>% 
  summarise(cft_score = mean(cft_score), 
            com_score = mean(com_score), 
            opt_score = mean(opt_score), 
            nobs = n())





set.seed(100)
# Step 1: Get row numbers for the training data
trainRowNumbers <- createDataPartition(df$working, p=0.8, list=FALSE)

# Step 2: Create the training  dataset
trainData <- heart[trainRowNumbers,]

# Step 3: Create the test dataset
testData <- heart[-trainRowNumbers,]
