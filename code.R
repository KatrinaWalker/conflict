library(rJava)
library(DataLoader)
library(tidyverse)
library(glmnet)
library(caret)
library(pROC)

setwd("~/Desktop/topics")
rm(list = ls())

#################################### Explore Data

# load files -- is there a sexier way to do this?
q1.2012 = read.csv("mergedfile_q1_y2012.csv")
q2.2012 = read.csv("mergedfile_q2_y2012.csv")
q4.2011 = read.csv("mergedfile_q2_y2012.csv")
q4.2016 = read.csv("mergedfile_q4_y2016.csv")

# types of variables
print(str(q1.2012))
names(q1.2012)

# how to unlist and save them as seperate dataframes
#xfiles <- lapply(Sys.glob("mergedfile_q[]_y201[]*.csv"), read.csv)
# mapply(assign, paste0("q.", 1:21),
       #files, MoreArgs = list(envir= .GlobalEnv))

### treat NA
# na.df = as.data.frame(lapply(q1.2012, function(x) ifelse(is.na(x), 0, 1)))

# how R is treating NA 
play = na.omit(q1.2012) # becomes 5612 x 161 dataframe (20% of original df)


# check na total in columns
na.count.col = as.data.frame(sapply(q1.2012, function(x) sum(is.na(x)))) # column
na.count.col$per = na.count.col$`sapply(q1.2012, function(x) sum(is.na(x)))` / 28076
na.count.col.50 = na.count.col %>% # 16% 
  filter(na.count.col$per >= .5)

#drop features 68% or more missing
drops <- c("povertyrate1","povertyrate2", "stockmarket", "anticorruption")
q1.2012 = q1.2012[ , !(names(q1.2012) %in% drops)]

# check na total in rows
na.count.row = as.data.frame(rowSums(is.na(q1.2012))) # row
na.count.row$per = na.count.row$`rowSums(is.na(q1.2012))` / 164
na.count.row.50 = na.count.row %>% # 16% 
  filter(na.count.row$per >= .5)

# drop rows with 50% more NA
q1.2012$row = (rowSums(is.na(q1.2012))) # rows
q1.2012$per = q1.2012$row / 164
q1.2012 = q1.2012 %>%
  filter(q1.2012$per <=.5)

###################################### Reclass variables

# turn factors in dummies (glmnet packages does not do this)
q1.2012Dummy <- dummyVars("~.",data=q1.2012, fullRank=F)
q1.2012 <- as.data.frame(predict(q1.2012Dummy,q1.2012))

# prop of outcome var
prop.table(table(q1.2012$ons_armedconf_best))

outcomeName <- 'ons_armedconf_bes'
predictorsNames <- names(q1.2012)[names(q1.2012) != outcomeName]

# outcome variable to factor
#q1.2012$ons_armedconf_best <- as.factor(q1.2012$ons_armedconf_best)

###################################### Model
names(getModelInfo())
getModelInfo()$gbm$type

# change outcome var to factor
q1.2012$ons_armedconf_best2 <- ifelse(q1.2012$ons_armedconf_best==1,'yes','nope')
q1.2012$ons_armedconf_best2 <- as.factor(q1.2012$ons_armedconf_best2)
outcomeName <- 'ons_armedconf_best2'

###################################### Partition Data
# create test set using quarter 4
testDF <- q1.2012 %>%
  filter(q1.2012$quarter == "4")

# create training set using quarter 3 2011
trainDF <- q1.2012 %>%
  filter(q1.2012$quarter != "4")

prop.table(table(testDF$ons_armedconf_best2))
prop.table(table(trainDF$ons_armedconf_best2))


###################################### Tune
objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

objModel <- train(trainDF[,predictorsNames], trainDF[,outcomeName], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

summary(objModel)

###################################### Run Logisitic Model

# including all features
model <- glm(ons_armedconf_best ~ . -1, data = train, family = binomial()) # obtain coefficients

# test glm on one variable
model <- glm(ons_armedconf_best ~ ste_theta0, data = train, family = binomial())
summary(model)
confint(model)
predict(model, type="response", newdata = test) # define treshold using a confusian matrix

res = residuals(model, type="deviance") # residuals

# dummy vars used in HMs models: anyviolence recoverystartquarter recoverystartphase recoveryphase longrecoveryphase
# thetas: ste_theta0-ste_theta8 and roll_theta0-roll_theta8
hm.df = select(q1.2012, year, quarter, isocode,ons_armedconf_best, ste_theta0, ste_theta8, roll_theta8, roll_theta0, anyviolence, recoverystartquarter, recoverystartphase, recoveryphase, longrecoveryphase)

# how R is treating NA in hm df 
play = na.omit(hm.df) # becomes 16903 x 9 dataframe (60% of original df)

na.count.col = as.data.frame(sapply(hm.df, function(x) sum(is.na(x)))) # column
na.count.col$per = na.count.col$`sapply(hm.df, function(x) sum(is.na(x)))` / 28076



