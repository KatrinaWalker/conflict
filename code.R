library(rJava)
library(DataLoader)
library(tidyverse)

setwd("~/Desktop/topics")
rm(list = ls())

#################################### Load Data

# load files -- is there a sexier way to do this?
q1.2012 = read.csv("mergedfile_q1_y2012.csv")
q2.2012 = read.csv("mergedfile_q2_y2012.csv")
q4.2011 = read.csv("mergedfile_q2_y2012.csv")


# maybe like this? but then how do you unlist and save them as seperate dataframes
xfiles <- lapply(Sys.glob("mergedfile_q[]_y201[]*.csv"), read.csv)
mapply(assign, paste0("q.", 1:21),
       files, MoreArgs = list(envir= .GlobalEnv))

###################################### Partition Data

# create test set using quarter 4 2011
test <- q1.2012 %>%
  filter(q1.2012$quarter == "4")

# create training set using quarter 3 2011
train <- q1.2012 %>%
  filter(q1.2012$quarter != "4")

###################################### Run Logisitic Model
# get coefficents

train$ons_armedconf_best <- as.factor(train$ons_armedconf_best)

predict <- glm(ons_armedconf_best ~ . -ons_armedconf_best, data = train, family = binomial())
summary(predict)



