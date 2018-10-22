#Title: predict.R
#Authors: Ravi, James, Lauren
#Description: Make predictions with stop-and-frisk data

#Load in libraries/data
require(tidyverse)

filelocation <- "./Data/sqf_08_16.csv"
sqf_08_16 <- read_csv(filelocation)

standardize <- function(x) {
  x.std <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  x.std
}

#-------------------------------------------------------

#A) Restrict to stops where the suspected.crime is 'cpw', then train a logistic regression model on all of 2008, 
# predicting whether or not a weapon is found. Use the following features as predictors, standardizing 
# real-valued attributes:

#Filter to only to crime = cpw
sqf.data <- sqf_08_16 %>% filter(suspected.crime=='cpw')
#Filter to only year = 2008 and standardize real-valued variables. This is our training set.
sqf.data.2008 <- sqf.data %>% filter(year == 2008) %>%
  mutate(suspect.height = standardize(suspect.height),
         suspect.weight = standardize(suspect.weight),
         suspect.age = standardize(suspect.age),
         observation.period = standardize(observation.period))
#Filter to only year = 2009 and standardize real-valued variables. This is our test set.
sqf.data.2009 <- sqf.data %>% filter(year == 2009) %>%
  mutate(suspect.height = standardize(suspect.height),
         suspect.weight = standardize(suspect.weight),
         suspect.age = standardize(suspect.age),
         observation.period = standardize(observation.period))

#Logistic Model
sqf2008_model <- glm(formula = found.weapon ~ 1 + precinct + location.housing + additional.report +
                     additional.investigation + additional.proximity + additional.evasive + 
                     additional.associating + additional.direction + additional.highcrime + additional.time +
                     additional.sights + additional.other + stopped.bc.object + stopped.bc.desc +
                     stopped.bc.casing + stopped.bc.lookout + stopped.bc.clothing + stopped.bc.drugs +
                     stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge + stopped.bc.other +
                     suspect.age + suspect.sex + suspect.build + suspect.height + suspect.weight + 
                     inside + radio.run + observation.period + day + month + time.period, 
                     family = "binomial",data = sqf.data.2008)


#III. Compute AUC of this model on 2009 data
#Generate predicted probabilities on the test set (2009 data)
sqf.data.2009$predicted.probability <- predict(sqf2008_model, newdata = sqf.data.2009, type='response')

#AUC
test.pred <- prediction(sqf.data.2009$predicted.probability, sqf.data.2009$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', 100*test.perf@y.values[[1]], "\n")

#IV. Draw 10000 random pairs from 2009
set.seed(2009)
True.2009.sample <- sample(which(sqf.data.2009$found.weapon == T),10000)
False.2009.sample <- sample(which(sqf.data.2009$found.weapon ==F),10000)

#Threshold?? = 0.5??


#-------------------------------------------------------
#B) Using the same model from part A, make a plot where the x-axis shows each year from 2009-2016, and the 
# y-axis shows the model AUC (computed using the ROCR package) when that year is used as the test set. 
# Explain in a few sentences what you observe, why you think this might be happening, and what one might 
# do about it.

#Create a dataframe with the two axis
model2008_AUCs <- data.frame(year = 2009:2016, AUC = rep(NA,8))

#Calculate AUC for every year
for(i in model2008_AUCs$year){
  temp.df <- sqf.data %>% filter(year == i) %>%
              mutate(suspect.height = standardize(suspect.height),
                    suspect.weight = standardize(suspect.weight),
                    suspect.age = standardize(suspect.age),
                    observation.period = standardize(observation.period))
  
  test.predicted.probability <- predict(sqf2008_model, newdata = temp.df, type='response')
  test.pred <- prediction(test.predicted.probability, temp.df$found.weapon)
  test.perf <- performance(test.pred, "auc")
  
  model2008_AUCs[which(model2008_AUCs$year == i),2] <- test.perf@y.values[[1]]
}

#Plot
theme_set(theme_bw())
p <- ggplot(data=model2008_AUCs, aes(y=AUC, x=factor(year)))
p +
  geom_point()+
  xlab("year")

#-------------------------------------------------------
#C) For this question, you will generate a performance and calibration plot (like the ones created in lecture 
# 6) for a classifier of your choice by following the steps below. You must repeat this once for each team member 
# (e.g., if there are two people on your team, you must choose two classifiers and generate a performance
# and calibration plot for each). Write at least one paragraph (per classifier) explaining what you did 
# and what you found. 




