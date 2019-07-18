# Here is where we load libraries
library(lubridate)
library(tidyverse)


# LOAD DATA ====

df <- read.csv("data/raw/teaching_training_data.csv")

table(df$gender)

unique_individuals <- df %>% 
  distinct(unid, gender)

view(df)

df <- df %>% 
  mutate(age_at_survey = interval(dob, survey_date_month)/years(1)) %>% 
  mutate(age = floor(age_at_survey))

#q1
df_q1 <- df %>% 
  group_by(gender) %>% 
  summarise(avg_age = mean(age, na.rm = TRUE)) %>% 
  ungroup()

#q2  
df_q2 <- df %>% 
  group_by(survey_num) %>% 
  summarise(count = n())

#q3
table(all_survey$gender)
all_survey <- df 
  
table(unique_individuals$gender)
unique_individuals <- df %>% 
  distinct(unid, gender)
  
#q4  
df_q <- df %>% 
  mutate(job_time = interval(job_start_date, job_leave_date)/months(1)) %>% 
  mutate(months_at_job = floor(job_time)) %>%

  mean(df_q$months_at_job, na.rm = TRUE)

#q5
ggplot(data = df) +
  geom_bar(mapping = aes(x = numearnincome, fill = volunteer))

#the lower numearnincome, the more likely to volunteer, probably has more time

#q6

#q7
ggplot(data = df) +
  geom_density(mapping = aes(x = peoplelive))
ggplot(data = df) +
  geom_density(mapping = aes(x = peoplelive_15plus))
ggplot(data = df) +
  geom_density(mapping = aes(x = numchildren))

#not reliable :(

#q8
ggplot(data = df) +
  geom_bar(mapping = aes(x = peoplelive, fill = gender), position = 'dodge')

#women are more likely to live in larger households

#q9
ggplot(data = df) +
  geom_bar(mapping = aes(x = numchildren, fill = gender), position = 'dodge')

#women are more likely to have children

#q10
ggplot(data = df) +
#  geom_smooth(mapping = aes(x = survery_date_month, job_leave_date)))

