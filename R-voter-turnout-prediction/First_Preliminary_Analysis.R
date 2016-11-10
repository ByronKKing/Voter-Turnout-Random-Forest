# Prepare first dataset for analysis

##make voter history factor variables, nan/unknown are missing
setwd('/Users/peterking/Desktop/0ptimus Project')
original = read.csv("voterfile.csv")
temp<-original
voterhist<-temp[12:26]
temp<-temp[,-(12:26),drop=FALSE]
voterhist[] <- lapply(voterhist, factor, 
                      levels=c(0, 1), 
                      labels = c("No", "Yes"))
temp<-data.frame(temp,voterhist)
is.na(temp) <- temp=="nan"
is.na(temp) <- temp=="Unknown"

##delete pets, instruments, nascar
analysis1<-temp[ , -which(names(temp) %in% c("intrst_nascar_in_hh","intrst_musical_instruments_in_hh","petowner_dog",
                                             "donates_to_liberal_causes","donates_to_conservative_causes","occupationindustry"))]

##modify dataset--create dataset w/ vars. below 20,000 missing values
analysis1<-analysis1[colSums(is.na(analysis1)) < 20000]

##create dataset w/ complete cases of these variables for random forest
###contains 7 vars that have less than 20,000 missing vars.
analysis1<-analysis1[complete.cases(analysis1),]

##code response variable
analysis1$gen12= ifelse (analysis1$vh12g=="Yes", 7, 0)
analysis1$gen10= ifelse (analysis1$vh10g=="Yes", 6, 0)
analysis1$gen08= ifelse (analysis1$vh08g=="Yes", 5, 0)
analysis1$gen06= ifelse (analysis1$vh06g=="Yes", 4, 0)
analysis1$gen04= ifelse (analysis1$vh04g=="Yes", 3, 0)
analysis1$gen02= ifelse (analysis1$vh02g=="Yes", 2, 0)
analysis1$gen00= ifelse (analysis1$vh00g=="Yes", 1, 0)
analysis1$genhist<-rowSums( analysis1[,29:35] )

##find median, if above median then 1, if not 0
summary(analysis1$genhist)
analysis1$turnout= ifelse (analysis1$genhist>10, 1, 0)
analysis1$turnout=as.factor(analysis1$turnout)
summary(analysis1$turnout)

##delete gen voter history variables
analysis1<-analysis1[ , -which(names(analysis1) %in% c("vh12g","vh10g","vh08g",
                                             "vh06g","vh04g","vh02g",
                                             "vh00g","gen12","gen10","gen08",
                                             "gen06","gen04","gen02","gen00","genhist"))]



# Run random forest algorithm

##set seed for replicable results
set.seed(1001)

##create train and test data
library(caret)
trainindex <- createDataPartition(analysis1$turnout, p = .8,
                                  list = FALSE,
                                  times = 1)
analysis1Train <- analysis1[ trainindex,]
analysis1Test  <- analysis1[-trainindex,]

##run random forest algorithm
library(randomForest)
RF1 <- randomForest(as.factor(turnout) ~ age + party + ethnicity + income + cd + dma + 
                      g08_precinct_turnout + g10_precinct_turnout + g12_precinct_turnout +
                      p08_precinct_turnout + p10_precinct_turnout + p12_precinct_turnout +
                      vh14p + vh12p + vh10p + vh08p + vh06p + vh04p + vh02p + vh00p,
                    data=analysis1Train, importance=TRUE, ntree=100)

##plot important variables
varImpPlot(RF1)

##predict on test data
Prediction <- predict(RF1, analysis1Test)
RFPRED <- data.frame(optimus_id = analysis1Test$optimus_id, 
                     turnout_old= analysis1Test$turnout,
                       turnout_new = Prediction)
