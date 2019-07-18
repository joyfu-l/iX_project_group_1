library(dplyr)
library(tidyverse)
library(lubridate)


df <- read.csv("data/raw/teaching_training_data.csv")
cft <- read.csv("data/raw/teaching_training_data_cft.csv")
com <- read.csv("data/raw/teaching_training_data_com.csv")
grit <- read.csv("data/raw/teaching_training_data_grit.csv")
num <- read.csv("data/raw/teaching_training_data_num.csv")
opt <- read.csv("data/raw/teaching_training_data_opt.csv")

cft <- cft[, -1]
com <- com[, -1]
grit <- grit[, -1]
num <- num[, -1]
opt <- opt[, -1]
df <- df[, -1]

class(cft$cft_score)
class(com$com_score)
class(grit$grit_score)
class(num$num_score)
class(opt$opt_score)


cft <- cft %>% 
  select(unid, cft_score) %>% 
  distinct(unid, .keep_all = TRUE)

# we want to do this for all 5 asssessments
# but it is a pain to copy and paste and then change inputs
# perfect opportunity for us to write our own function
# See Jenny Bryan's tutorials starting here (http://stat545.com/block011_write-your-own-function-01.html)

# making function
# keep all in reference to other

helper_function <- function(file_name) {
  file_name %>% 
    select(1:2) %>% 
    distinct(unid, .keep_all = TRUE)
}

grit <- helper_function(grit)
com <- helper_function(com)
num <- helper_function(num)
opt <- helper_function(opt)

df_assess <- df_assess %>% 
  left_join(cft, by = "unid") %>%
  left_join(com, by = "unid") %>%
  left_join(grit, by ="unid") %>% 
  left_join(num, by ="unid") %>% 
  left_join(opt, by ="unid") %>%

left_join("cft", "com", "grit", "num", "opt", by = unid)


## PLAN

# mutate time worked back in
df <- df %>% 
  mutate(time_at_job = interval(job_start_date, job_leave_date)/months(1)) %>% 
  mutate(jobtime = floor(time_at_job))

# working as contingent on grit, number of children, time worked (months), and cft
# get rid of NAs
minus_na <- df %>% 
  filter(!is.na(grit_score) & !is.na(numchildren) & !is.na(jobtime) & !is.na(cft_score))

# need to scale grit and cft
#which step goes first idk!!! sep ???


# then add in children and time worked
# do sep and then merge together then ???

# split by gender (???)
# then by age groups (??)

# set seed
set.seed(1234)

# train Data
trainData <- df[trainRowNumbers,]

# test Data
testData <- df[-trainRowNumbers,]

# then do the thing with the how much variables mattered

# clean up data
  # iteratively- subset observations of every variable
  # then run model on soemthing wiht no NAs
    #filter
# descriptives on variables
# get it to stage to run models

