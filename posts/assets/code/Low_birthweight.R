#NECESSARY STARTUP
#Load dataset into R
babies = read.csv("C:/Users/Logan/Downloads/Sample-Data-Birth-Weight-Risk.txt")
library(stringr)

#DATA CLEANING
#Because three of the numeric variables are entered in the file with surrounding text,
#we need to remove the text and tell R to interpret them as numeric variables, not factors. 
BWT = babies$BWT
BWT = str_sub(BWT, 10, -11)
BWT = as.numeric(str_match(BWT,"[0-9]+"))

Age = babies$Age
Age = str_sub(Age, 10, -11)
Age = as.numeric(str_match(Age,"[0-9]+"))

LWT = babies$LWT
LWT = str_sub(LWT, 10, -11)
LWT = as.numeric(str_match(LWT,"[0-9]+"))

#Three of the variables are binary and entered as True/False. 
#The code below converts each False entry to 0 and each True entry to 1. 
Smoker = babies$Smoker
Smoker = as.numeric(Smoker)
#By default, R codes False=1 and True=2, but we want False=0 and True=1. 
for(i in 1:length(Smoker)){ 
  Smoker[i] = Smoker[i] - 1}

Low = babies$Low
Low = as.numeric(Low)
for(i in 1:length(Low)){
  Low[i] = Low[i] - 1}

Hypertension = babies$Hypertension
Hypertension = as.numeric(Hypertension)
for(i in 1:length(Hypertension)){
  Hypertension[i] = Hypertension[i] - 1}

UI = babies$UI
UI = as.numeric(UI)
for(i in 1:length(UI)){
  UI[i] = UI[i] - 1}

#The race variable is scored 1, 2, 3 in the data. Since a priori we have no reason to believe
#that the races are ordered in terms of susceptibility to giving birth to low-weight babies,
#we need to encode this variable's information without the 1, 2, 3 ordering. 
#One way to do this is to create dummy variables - two separate variables that indicate if 
#a person is of race '2' or of race '3'. We use race '1' as the default because it is the most 
#common in our sample. 
Race = babies$Race
Race2 = c()
Race3 = c()
for(i in 1:length(Race)){
  if(Race[i] == 2){Race2 = c(Race2, 1)}
  else {Race2 = c(Race2, 0)}}

for(i in 1:length(Race)){
  if(Race[i] == 3){Race3 = c(Race3, 1)}
  else {Race3 = c(Race3, 0)}}

#The remaining variables require no cleaning. 
PTL = babies$PTL
FTV = babies$FTV

#Now that we've cleaned everything up, let's 
#glue the data back together in a new data.frame. 
#This will keep everything organized in case we want it later. 
babies = data.frame("Low" = Low, "BWT" = BWT, "Age" = Age, "LWT" = LWT, "Smoker" = Smoker, 
                    "Hypertension" = Hypertension, "UI" = UI, "Race2" = Race2,
                    "Race3" = Race3, "PTL" = PTL, "FTV" = FTV)

#We can get a quick overview of the data by plotting all the variables pairwise and by 
#generating the covariance matrix. 
plot(babies)
cov(babies)

#Blindly run a regression with all predictor variables included: 
fit = lm(BWT ~ Age + LWT + Smoker + Hypertension + UI + Race + PTL + FTV)
summary(fit)
#If the models were reasonably good (e.g. r^2 > 0.7), we might impose some penalties on the regression
#(as in LASSO) to balance the predictive value of the model with its complexity. 
#But in this case the model is uselessly poor, so there is no point in constraining it. 

#Since the linear model didn't work, let's try a piecewise linear approach. 
#Classification and regression trees allow us to divide the data into bins and generate 
#a prediction for each bin. Although there is no consideration of the variance
#within bins, this method allows us to treat different pockets of the dataset differently. 
#We'll start with a regression tree to compare to the linear model. 
library(rpart)
tree1 = rpart(formula = BWT ~ Age + LWT + Smoker + Hypertension + UI + Race + PTL + FTV,
             data = babies, method = "anova")
rsq.rpart(tree1)
plot(tree1)
text(tree1)

#We can also grow a classification tree. 
tree2 = rpart(formula = Low ~ Age + LWT + Smoker + Hypertension + UI + Race + PTL + FTV,
              data = babies, method = "class")
plot(tree2)
text(tree2)


