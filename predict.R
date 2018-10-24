#Title: predict.R
#Authors: Ravi, James, Lauren
#Description: Make predictions with stop-and-frisk data

#Load in libraries/data
require(tidyverse)
require(data.table)

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

#Convert variable types as necessary
sqf.data <- sqf.data %>% mutate(suspect.race = as.factor(suspect.race), 
                                suspect.build = as.factor(suspect.build),
                                suspect.sex = as.factor(suspect.sex),
                                location.housing = as.factor(location.housing),
                                day = as.factor(day),
                                month = as.factor(month),
                                time.period = as.factor(time.period),
                                precinct = as.factor(precinct))
#Standardize real-valued variables
sqf.data <- sqf.data %>% 
  mutate(suspect.height = standardize(suspect.height),
         suspect.weight = standardize(suspect.weight),
         suspect.age = standardize(suspect.age),
         observation.period = standardize(observation.period))

#Filter to only year = 2008. This is our training set.
sqf.data.2008 <- sqf.data %>% filter(year == 2008)

#Filter to only year = 2009. This is our test set.
sqf.data.2009 <- sqf.data %>% filter(year == 2009)

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
#I. Ten largest, ten smallest coefficients
#Smallest
sqf2008_model$coefficients %>%
  data.frame(VarName = names(.),Coefficients = .) %>%
  arrange(Coefficients) %>%
  slice(1:10)
#Largest
sqf2008_model$coefficients %>%
  data.frame(VarName = names(.),Coefficients = .) %>%
  arrange(desc(Coefficients)) %>%
  slice(1:10)

#The log odds of a stopped person carrying a weapon is increased by 2.59 if they are stopped in transit
#compared to if they were stopped at home.

#II.
results1 <- as.data.frame(coef(sqf2008_model))
results <-transpose(results1)
colnames(results) <- rownames(results1)
names(results)[1]<-"Intercept"


#plug values into  logit
logit = results$Intercept + results$location.housingtransit + results$additional.highcrimeTRUE + results$stopped.bc.bulgeTRUE + (results$suspect.age*age ) + (results$suspect.height*height)+ (results$suspect.weight*weight) + results$suspect.buildmedium + results$observation.period*op  + results$time.period6 + results$`month^10` + results$`day^4` 

#calculate probability for male
odds <- exp(logit)

cat('the probability that he would be found with a weapon is', (odds / (1 + odds))) 
#I don't think we are supposed to calculate the formula by substracting from 1, but otherwise the odds are way too high...

#plug values into  logit (adding coefficient for female)
logitf = results$Intercept + results$location.housingtransit + results$additional.highcrimeTRUE + results$stopped.bc.bulgeTRUE + (results$suspect.age*age ) + (results$suspect.height*height)+ (results$suspect.weight*weight) + results$suspect.buildmedium + results$observation.period*op + results$radio.runTRUE  + results$time.period6 + results$`month^10` + results$`day^4` + results$suspect.sexfemale 

odds1 <- exp(logitf)

cat(', the probability that SHE would be found with a weapon is', (odds1 / (1 + odds1))) 


#III. Compute AUC of this model on 2009 data
#Generate predicted probabilities on the test set (2009 data)
sqf.data.2009$predicted.probability <- predict(sqf2008_model, newdata = sqf.data.2009, type='response')

#AUC
test.pred <- prediction(sqf.data.2009$predicted.probability, sqf.data.2009$found.weapon)
test.perf <- performance(test.pred, "auc")
cat('the auc score is ', 100*test.perf@y.values[[1]], "\n")

#IV. Draw 10000 random pairs from 2009
set.seed(2009)
not.na.idxs <- which(!is.na(sqf.data.2009$predicted.probability))
not.na.rows.2009 <- sqf.data.2009[not.na.idxs,]
found.weapon.idxs <- which(not.na.rows.2009$found.weapon == TRUE)
notfound.weapon.idxs <- which(not.na.rows.2009$found.weapon == FALSE)
  
True.2009.sample <- sample(found.weapon.idxs, 10000, replace = TRUE)
False.2009.sample <- sample(notfound.weapon.idxs, 10000, replace = TRUE)

ProbCompare <- sqf.data.2009[True.2009.sample,'predicted.probability'] > sqf.data.2009[False.2009.sample,"predicted.probability"]
sum(ProbCompare)/nrow(ProbCompare)


#-------------------------------------------------------
#B) Using the same model from part A, make a plot where the x-axis shows each year from 2009-2016, and the 
# y-axis shows the model AUC (computed using the ROCR package) when that year is used as the test set. 
# Explain in a few sentences what you observe, why you think this might be happening, and what one might 
# do about it.

#Create a dataframe with the two axis
model2008_AUCs <- data.frame(year = 2009:2016, AUC = rep(NA,8))

#Calculate AUC for every year
for(i in model2008_AUCs$year){
  temp.df <- sqf.data %>% filter(year == i)
  
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


#James read in filter to arrest.reasons 
James.data <- sqf_08_16 %>% filter(grepl('marihuana|substance',suspected.crime))

#Convert variable types to factors as necessary and standardize real-value variables
James.data <- James.data %>% 
  mutate(suspect.race = as.factor(suspect.race), 
         suspect.build = as.factor(suspect.build),
         suspect.sex = as.factor(suspect.sex),
         location.housing = as.factor(location.housing),
         day = as.factor(day),
         month = as.factor(month),
         time.period = as.factor(time.period),
         precinct = as.factor(precinct)) %>%
  mutate(suspect.height = standardize(suspect.height),
         suspect.weight = standardize(suspect.weight),
         suspect.age = standardize(suspect.age),
         observation.period = standardize(observation.period))

#Set training data to 2008-2010
James.training <- James.data %>% filter(year == 2008:2010)

#Set test data to 2011
James.test <- James.data %>% filter(year == 2011)

#Logistic Model
James_model <- glm(formula = found.contraband ~ 1 + precinct + location.housing + additional.report +
                     additional.investigation + additional.proximity + additional.evasive + 
                     additional.associating + additional.direction + additional.highcrime + additional.time +
                     additional.sights + additional.other + stopped.bc.object + stopped.bc.desc +
                     stopped.bc.casing + stopped.bc.lookout + stopped.bc.clothing + stopped.bc.drugs +
                     stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge + stopped.bc.other +
                     suspect.age + suspect.sex + suspect.build + suspect.height + suspect.weight +
                     suspect.race + inside + radio.run + observation.period + day + month + time.period, 
                   family = "binomial",data = James.training)

#Generate predictions for test set
James.test$predicted.probability <- predict(James_model,newdata = James.test,type = 'response')

#Performance plot
plot.data <- James.test %>% arrange(desc(predicted.probability)) %>% 
  mutate(numstops = row_number(), percent.outcome = cumsum(found.contraband)/sum(found.contraband),
         stops = numstops/n()) %>% select(stops, percent.outcome)

#create plot
theme_set(theme_bw())
James.p1 <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
James.p1 <- James.p1 + geom_line()
James.p1 <- James.p1 + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                                     labels=c('0.3%','1%','3%','10%','30%','100%'))
James.p1 <- James.p1 + scale_y_continuous("Percent of contraband recovered", limits=c(0, 1), labels=scales::percent)
James.p1

#Calibration plot
plot.data <-James.test %>% 
  mutate(rounded.pred = round(predicted.probability,digits = 2)*100) %>%
  group_by(rounded.pred) %>%
  summarise(model.estimate = mean(predicted.probability), 
            numstops = n(),
            empirical.estimate = mean(found.contraband))

#create plot
James.p2 <- ggplot(data = plot.data, aes(y = empirical.estimate, x = model.estimate))
James.p2 <- James.p2 + geom_point(aes(size = numstops), alpha = 0.5)
James.p2 <- James.p2 + scale_size_area(guide='none', max_size=15)
James.p2 <- James.p2 + geom_abline(intercept=0, slope=1, linetype="dashed")
James.p2<- James.p2 + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                                    labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
James.p2 <- James.p2 + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                                     labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
James.p2

#I decided to use found.contraband as my classifier. For this, I first subsetted the full sqf data to only include
#criminal possession or sale of 'marihuana' and 'controlled substance' as suspected reasons. We want to
#build a model to predict when in the situations when police stop someone with the suspicion of drugs, they find anything.
#The training set comprises of the data from 2008-2010 and the test data is 2011 data, with the assumption that going further
#out would make the results less accurate.
#I ran a logistic model on:
# -precinct;
# -whether the stop occurred in transit, housing, or on the street;
# -the ten additional stop circumstances (additional.*);
# -the ten primary stop circumstances (stopped.bc.*);
# -suspect age, race, build, sex, height , and weight;
# -whether the stop occurred inside, and whether the stop was the result of a radio call;
# -length of observation period;
# -day, month, and time of day
#The model is not obviously good. The performance plot shows that a little increase in stops is not a
#a one-to-one increase to the percentage of contraband recovered. The calibration plot shows that our
#model is mostly underestimating the actual found contrabands.


#C)Lauren C

#Filter to only year = 2012-2014. This is our training set.
sqf.data.12thru14 <- sqf.data %>% filter(year == 2012:2014) 
  
#Filter to only year = 2015/ This is is our test set.
sqf.data.2015 <- sqf.data %>% filter(year == 2015)%>% filter(force.wall != "NA")
  
#build model to predict whether an officer puts individual against a wall

force.wall_model <- glm(formula = force.wall ~ 1 +  location.housing + additional.report +
                     additional.investigation + additional.proximity + additional.evasive + 
                     additional.associating + additional.direction + additional.highcrime + additional.time +
                     additional.sights + additional.other + stopped.bc.object + stopped.bc.desc +
                     stopped.bc.casing + stopped.bc.lookout + stopped.bc.clothing + stopped.bc.drugs +
                     stopped.bc.furtive + stopped.bc.violent + stopped.bc.bulge + stopped.bc.other +
                     suspect.age + suspect.sex + suspect.build + suspect.height + suspect.weight + 
                     inside + radio.run + observation.period + day + month + time.period + city + suspect.race, 
                     family = "binomial", data = sqf.data.12thru14)

#predict probabilities
# 1. generate predictions for test set
sqf.data.2015$predicted.probability <- predict(force.wall_model, newdata = sqf.data.2015, type='response') 

# make performance plot
plot.data <- sqf.data.2015 %>% arrange(desc(predicted.probability)) %>% 
  mutate(numstops = row_number(), percent.outcome = cumsum(force.wall)/sum(force.wall),
         stops = numstops/n()) %>% select(stops, percent.outcome)

# create performance plot
theme_set(theme_bw())
pp <- ggplot(data=plot.data, aes(x=stops, y=percent.outcome)) 
pp <- pp + geom_line()
pp <- pp + scale_x_log10('\nPercent of stops', limits=c(0.003, 1), breaks=c(.003,.01,.03,.1,.3,1), 
                       labels=c('0.3%','1%','3%','10%','30%','100%'))
pp <- pp + scale_y_continuous("Percent of instances of suspect held against a wall", limits=c(0, 1), labels=scales::percent)
pp


# 3) make calibration plot
plot.data <- sqf.data.2015  %>% mutate(calibration = round(100*predicted.probability)) %>% 
  group_by(calibration) %>% summarize(model.estimate = mean(predicted.probability),
                                      numstops = n(),
                                      empirical.estimate = mean(force.wall))

# create and save plot
p <- ggplot(data = plot.data, aes(y=empirical.estimate, x=model.estimate))
p <- p + geom_point(alpha=0.5, aes(size=numstops))
p <- p + scale_size_area(guide='none', max_size=15)
p <- p + geom_abline(intercept=0, slope=1, linetype="dashed")
p <- p + scale_y_log10('Empirical probability \n', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p <- p + scale_x_log10('\nModel estimated probability', limits=c(.001,1), breaks=c(.001,.003,.01,.03,.1,.3,1), 
                       labels=c('0.1%','0.3%','1%','3%','10%','30%','100%'))
p




#
#------------------------------------------------------------------------------


