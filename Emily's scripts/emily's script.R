library(dplyr)

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

left_join("df", "cft", "com", "grit", "num", "opt", by = unid)


# clean up data
  # iteratively- subset observations of every variable
  # then run model on soemthing wiht no NAs
    #filter
# descriptives on variables
# get it to stage to run models

