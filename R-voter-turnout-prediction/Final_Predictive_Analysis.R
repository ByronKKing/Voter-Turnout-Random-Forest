#################prepare original dataset for imputation
#make voter history factor variables
setwd('/Users/peterking/Desktop/0ptimus Project')
original = read.csv("voterfile.csv")
temp<-original
voterhist<-temp[12:26]
temp<-temp[,-(12:26),drop=FALSE]
voterhist[] <- lapply(voterhist, factor, 
                      levels=c(0, 1), 
                      labels = c("No", "Yes"))
temp<-data.frame(temp,voterhist)
#make nan/unknown are missing
is.na(temp) <- temp=="nan"
is.na(temp) <- temp=="Unknown"
imputation<-temp
#build voter history score
temp$gen12= ifelse (temp$vh12g=="Yes", 7, 0)
temp$gen10= ifelse (temp$vh10g=="Yes", 6, 0)
temp$gen08= ifelse (temp$vh08g=="Yes", 5, 0)
temp$gen06= ifelse (temp$vh06g=="Yes", 4, 0)
temp$gen04= ifelse (temp$vh04g=="Yes", 3, 0)
temp$gen02= ifelse (temp$vh02g=="Yes", 2, 0)
temp$gen00= ifelse (temp$vh00g=="Yes", 1, 0)
temp$genhist<-rowSums( temp[,40:46] )
summary(temp$genhist)
#code all general history scores above 10 as 1, below as 0
temp$turnout= ifelse (temp$genhist>10, 1, 0)
temp$turnout=as.factor(temp$turnout)
str(temp)
optimusdata<-temp

#################impute age using decision tree
#call rpart package
set.seed(8428)
library(rpart)
#####create imputation model
str(optimusdata$age)
summary(optimusdata$age)
age_fit <- rpart(age ~ ethnicity + income + party, 
                 data=optimusdata[!is.na(optimusdata$age),], method="anova")
optimusdata$age[is.na(optimusdata$age)] <- predict(age_fit, optimusdata[is.na(optimusdata$age),])
summary(optimusdata$age)
str(optimusdata)

#################build final model
#keep only variables used in model
optimusdata<-optimusdata[ , which(names(temp) %in% c("optimus_id","age","g08_precinct_turnout","g10_precinct_turnout", 
                                                     "g12_precinct_turnout","p08_precinct_turnout", "p10_precinct_turnout",
                                                     "p12_precinct_turnout","vh14p","vh12p","vh10p","vh08p","vh06p","vh04p",
                                                     "turnout"))]
#impute manually remaining missing data point
optimusdata[27307,]<-c(10756,68,0,0,0,0,0,0,"No","No","No","No","No","No",0)
optimusdata[27307,]
#run model
set.seed(4280)
library(randomForest)
rand_forest <- randomForest(as.factor(turnout) ~ age + g08_precinct_turnout + g10_precinct_turnout + 
                      g12_precinct_turnout + p08_precinct_turnout + p10_precinct_turnout + 
                      p12_precinct_turnout + vh14p + vh12p + vh10p + vh08p + vh06p + vh04p,
                    data=optimusdata, importance=TRUE, ntree=100)

#################create data frame with predictions and probabilities
Optimus_voter_prediction <- predict(rand_forest, optimusdata)
Optimus_voter_probability <- predict(rand_forest, optimusdata,type="prob")
attach(optimusdata)
rand_forest_prediction<-data.frame(optimus_id = optimusdata$optimus_id,age,
                    g08_precinct_turnout,g10_precinct_turnout,g12_precinct_turnout,
                    p08_precinct_turnout,p10_precinct_turnout,p12_precinct_turnout,
                    vh14p,vh12p,vh10p,vh08p,vh06p,vh04p,
                    vote=Optimus_voter_prediction,vote_prob = Optimus_voter_probability)
detach(optimusdata)
finalprediction<-rand_forest_prediction[,-16]
names(finalprediction)[names(finalprediction)=="vote_prob.1"]<-"vote_prob"
summary(finalprediction$vote)

#analysis of those who turned out
voterturnout_2014<-finalprediction[finalprediction$vote==1,]
summary(voter_turnout2014)

write.csv(finalprediction, '/Users/peterking/Desktop/Optimus Project/optimus_modeling_exercise.csv")

