#read file into 'original'
setwd('/Users/peterking/Desktop/0ptimus Project')
original = read.csv("voterfile.csv")
str(original)
#create temp from original, make voter history factor variables
temp<-original
voterhist<-temp[12:26]
temp<-temp[,-(12:26),drop=FALSE]
voterhist[] <- lapply(voterhist, factor, 
               levels=c(0, 1), 
               labels = c("No", "Yes"))
temp<-data.frame(temp,voterhist)

###########missing data analysis using VIM package
library(VIM)
#convert nan and 'unknown' to missing
is.na(temp) <- temp=="nan"
is.na(temp) <- temp=="Unknown"
#call missing data frequency per variable and plot missing data patterns
missing<-aggr(temp,bars=FALSE,combined=TRUE,sortVars=FALSE)
missing
#drop voter histories (no missing values) and turnout probs (1 missing value each)
simple<-temp[,-(25:39),drop=FALSE]
simpler<-simple[,-(19:24),drop=FALSE]
###call missing data frequency again
missing<-aggr(simpler,bars=FALSE,combined=TRUE,sortVars=FALSE)
missing
#delete all data impossible to impute (pets,nascar,instruments)
simpler<-simpler[,-(13:15),drop=FALSE]
#exclude variables with missing values above 40,000 (donates to liberal/conservative,occupationindustry)
simplest<-simpler[colSums(is.na(simpler)) < 40000]
###plot missing data freq again
#seems to be little trend in missing values--evidence for MCAR
missing<-aggr(simplest,bars=FALSE,combined=TRUE,sortVars=FALSE)
missing
###dataset with variables with missing values above 20,000
#seems to be little trend in vars with most missing values--evidence for MCAR
mostmissing<-simplest[colSums(is.na(simplest)) > 20000]
mostmiss<-aggr(mostmissing,bars=TRUE,numbers=FALSE,sortVars=TRUE,cex.axis=.7, gap=.7)
mostmiss
###dataset with variables with missing values below 20,000
#still seems to be little trend in vars with least missing values--evidence for MCAR
leastmissing<-simplest[colSums(is.na(simplest)) < 20000]
leastmiss<-aggr(leastmissing,bars=TRUE,numbers=FALSE,sortVars=TRUE,cex.axis=.7, gap=.7)
leastmiss
###any correlation with other particular vars?
#age and ethnicity--MCAR--no significant trend
x <- leastmissing[, c("age", "ethnicity")]
barMiss(x)
barMiss(x, only.miss = FALSE)
#age and income--MAR--voters in early 20s more likely to have missing income values...
y <- leastmissing[, c("age", "income")]
barMiss(y)
barMiss(y, only.miss = FALSE)
#ethnicity and income--MCAR--no significant missing data trend
z <- leastmissing[, c("ethnicity", "income")]
barMiss(z)
barMiss(z, only.miss = FALSE)

###########descriptive stats, general exploratory data analysis
#####party
summary(original$party)
df<-data.frame(table(original$party))
colnames(df)<-c('Party','Freq')
#pie chart with all parties
barplot<- ggplot(data=df, aes(x="", y=Freq, fill=Party))+geom_bar(width = 1, stat = "identity")
pie_parties <- barplot + coord_polar("y", start=0) +
  labs(title="Party Frequency") +
  theme(axis.text.x=element_blank())
#code parties other than dem and repub as 'other'
temp$party<-recode(temp$party, "'Democratic'='Democrat';'Republican'='Republican';
                    'American Independent'='Other';'Green'='Other';'Libertarian'='Other';
                    'Natural Law'='Other';'Non-Partisan'='Other';'Other'='Other'")
df1<-data.frame(table(temp$party))
colnames(df1)<-c('Party','Frequency')
#barchart with abridged party distinctions
ggplot(df1, aes(x=Party, y=Frequency, fill=Party))+
  geom_bar(width = 1, stat = "identity")+
  scale_fill_manual(values=c("blue","pink","red"))+
  labs(title="Party Frequency",x = "Party",y="Frequency")
#####ethnicity
summary(original$ethnicity)
df2<-data.frame(table(original$ethnicity))
colnames(df2)<-c('Ethnicity','Frequency')
ggplot(data=df2, aes(x=Ethnicity, y=Frequency, fill=Ethnicity)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(title="Ethnicity Frequency",x = "Ethnicity",y="Frequency")
#####age
summary(original$age)
ggplot(data=original, aes(original$age)) + 
  geom_histogram(binwidth=5, colour="black",fill="white")+
  labs(title="Age Distribution",x = "Age",y="Count")
#####age vs ethnicity
ggplot(data=original, aes(x=original$ethnicity, y=original$age,
                                   ,fill=original$ethnicity)) + 
            geom_boxplot()+
            labs(title="Ethnicity by Age",
             x = "Ethnicity",y="Age",fill="Ethnicity")
#####age vs party
ggplot(data=original, aes(x=original$party, y=original$age,
                                   ,fill=original$party)) + geom_boxplot() +
              labs(title="Party by Age", x = "Party",y="Age",fill="Party")
#w/ 'other' coding
attach(temp)
ggplot(data=temp, aes(x=party, y=age,
                                ,fill=party)) + geom_boxplot()+ 
  labs(title="Party by Age",x = "Party",y="Age",fill="Party")
detach(temp)
#####plot ethnicity vs. party
attach(temp)
contingency<-table(ethnicity,party)
ethnicity_party<-data.frame(contingency)
newbarchart<-ggplot(data=ethnicity_party, aes(x=ethnicity, y=Freq, fill=party)) +
  geom_bar(stat="identity", position=position_dodge())
newbarchart + labs(title="Party by Ethnicity",
                x = "Ethnicity",y="Frequency",fill="Party") +
                scale_fill_manual(values=c("blue","pink","red"))
#####general/primary turnout rates
#create data frame with turnout numbers
turnout_years<-as.data.frame(summary(temp[,25:39]))
#create function to extract total turnout number for each election type
library(gdata)
turnout_years$Freq <- as.character(turnout_years$Freq)
turnout_years$Freq[1]
trim(strsplit(turnout_years$Freq[1], split='[:]'))
trim(strsplit(turnout_years$Freq[1], split='[:]')[[1]][2])
turnout_years$turnout<-sapply(turnout_years$Freq, function(x) {trim(strsplit(x, split='[:]')[[1]][2])})
#delete unnecessary variables
turnout_years<-turnout_years[,-c(1,3)]
#select only number that turned out
turnout<- turnout_years[seq(2, nrow(turnout_years), by=2), ]
#extract general or primary
turnout$Var2<-as.character(turnout$Var2)
turnout$Var2[1]
substr(turnout$Var2[1],5,5)
turnout$election<-sapply(turnout$Var2, function(x) {substr(x, 5, 5)})
turnout$election<- ifelse(turnout$election == 'p', 'primary', 'general')
#extract year
turnout$year<-sapply(turnout$Var2, function(x) {substr(x, 3, 4)})
#convert chars to nums, delete missing variables
turnout$year<-as.numeric(turnout$year)
turnout$turnout<-as.numeric(turnout$turnout)
turnout<-turnout[,-1]
turnout$year<-turnout$year+2000
###plot linegraph
ggplot(data=turnout, aes(x=year, y=turnout, group=election, colour=election)) +
  geom_line() +
  geom_point()+
  labs(title="Turnout by Election",
       x = "Election Year",y="Turnout",fill="Electon Type")+
  geom_abline()
###plot bargraph
turnout_barchart<-ggplot(data=turnout, aes(x=year, y=turnout, fill=election)) +
  geom_bar(stat="identity", position=position_dodge())
turnout_barchart + labs(title="Turnout by Election",
                        x = "Election Year",y="Turnout",fill="Electon Type") +
  scale_fill_manual(values=c("blue","turquoise"))
###general election turnout
turnout_general<-turnout[ !c(TRUE,FALSE), ]
turnout_general
ggplot(data=turnout_general, aes(x=year, y=turnout, colour=election)) +
  geom_line() +
  geom_point()+
  labs(title="General Election Turnout",
       x = "Election Year",y="Turnout")+
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014)) +
  ylim(0,40000)






