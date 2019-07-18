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
