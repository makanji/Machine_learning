# loading and splitting up datas

mydata = read.table("breast-cancer_shuffled.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
countTraining = round(nrow(mydata)*0.7)
randomRows=sample(1:nrow(mydata), size= countTraining, replace=F)
dfTraining = mydata[randomRows,]
dfTest = mydata[- randomRows,]
dim(mydata)
library(randomForest)
#################################################################
### Read the data
mydata = read.table("breast-cancer_shuffled.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
### Check the structure of your data
str(mydata)
###########

### Split the data and select random observations 
randomRows=sample(1:nrow(mydata), size= countTraining, replace=F)
dfTraining = mydata[randomRows,]
dfTest = mydata[- randomRows,]
###########
             ### Construct a random forest using the training dat
myForest = randomForest(type~., dfTraining)
    print(myForest)


##########

## The library "randomForest" contains a function to investigate teh variable importance
varImpPlot(myForest)
x = ctree(class~., dfTraining)
############

## you can plot the error rates for each response variables

plot(myForest, log="y", col=1:3)
legend("topright", colnames(myForest$err.rate),col=1:3,cex=0.8,fill=1:3)
###########

myPred = predict(myForest, dfTest, type = "class")
#type= response', 'vote', 
table(myPred, dfTest$type)


######## another exercise
####################
#Import the data file called "forestTypeTraining.csv" into the variable myTraining
#Import the data file called "forestTypeTesting.csv" into the variable myTesting

#-Process the dataset
#-Create a random forest  for classifying the data based on the class with myTraining (function in R: randomForest)
#-Check the created forest and its performance on the training data
#-Create a confusion matrix for the predicted and true class on myTesting

#############
myTraining = read.table("forestTypeTraining.csv", header = TRUE, sep =",")
myTest = read.table("forestTypeTesting.csv", header = TRUE, sep =",")
myTraining$class = as.factor(myTraining$class)
myTest$class = as.factor(myTest$class)
##########
myForest = randomForest(class~., myTraining)
myForest 
########
myPred = predict(myForest, myTest, type = "class")
confT=table(myPred, myTest$class)
##########


#Confusion matrix is based on out-of-bag data and different from the one created by predict
myPred = predict(myForest, myTest, type = "class")
confT=table(myPred, myTest$class)

### Compute the correct predictions (= true positives)
tpRF = confT[1,1] + confT[2,2] + confT[3,3] + confT[4,4]
### Sum of correct predictions
sumAllRF = sum(confT)

### Compute the performance of the Random Forest (divide correct predictions by all predictions)
predictionPerfRF = tpRF / sumAllRF
predictionPerfRF

########## you can plot the error rates for each response variables
plot(myForest)
#############
## Look at variable importance:
varImpPlot(myForest)

#type= 1 or 2, specifying the type of importance measure 
#(1=mean decrease in accuracy, 2=mean decrease in node impurity)
round(importance(myForest, type=1), 2)
round(importance(myForest, type=2), 2)

#####################################################
############# Random Forest - The `unsupervised' case
myUnSupervisedRf= randomForest(myTraining[, -1])

MDSplot(myUnSupervisedRf, myTraining$class)

varImpPlot(myUnSupervisedRf)

## The `unsupervised' case:
tmpData=rbind(myTraining, myTest)
myUnSupervisedRf= randomForest(tmpData[, -1])
MDSplot(myUnSupervisedRf, tmpData$class)
varImpPlot(myUnSupervisedRf)
# unsupervised case: focusing on the distance metric obtained in the "proximities". 
# This should be an NxN matrix representing the times that the samples co-occur 
# in terminal nodes. 


###################################################################################
########################  TPR-FPR #################################################

Import the data file called "breast-cancer_shuffled.csv" into the variable mydata
- Process the dataset
- Divide mydata in two data frames as dfTraining (70%) and dfTest (30%)
-Perform a Random Forest (RF) classifier
-Plot the error rates
-Plot the variable importance
-Using dfTest create the confusion matrix and interpret the results
-Change the type parameter in predict function as "vote"

Step2:
  Consider the success probabilities for no-recurrence-events from 0 to 1 by an increment=0.01
Calculate TPR and and FPR values 100 times and plot them

######################

mydata = read.table("breast-cancer_shuffled.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
str(mydata)


countTraining=round(0.7*nrow(mydata))
randomRows=sample(1:nrow(mydata), size= countTraining, replace=F)
dfTraining = mydata[randomRows,]
dfTest = mydata[- randomRows,]

##############

myPred = predict(myForest, dfTest, type = "class")
#############

myPred = predict(myForest, dfTest, type = "vote")

head(myPred)

#########

# consider the success probabilities for no-recurrence-events 
# from 0 to 1 by an increment of 0.01
# calculate TPR and and FPR values 100 times and plot them

## Create a dataframe
predictions=as.data.frame(myPred) 

predictions$type=dfTest$type


## Create empty vectors for storing the tpr and fpr values
TPR=c()
FPR=c()

## define the starting value
startValue=0

#########

totalPositiveNumber=nrow(predictions[predictions$type=="no-recurrence-events",])

totalNegativeNumber=nrow(predictions)-totalPositiveNumber

summary(predictions)


########################
########################
for(i in 1:100){
  selectedRows=subset(predictions, 
                      predictions$`no-recurrence-events`>=startValue & predictions$type=="no-recurrence-events")
  
  TPR[i]=nrow(selectedRows) / totalPositiveNumber
  selectedRows2=subset(predictions, 
                       predictions$`no-recurrence-events` >= startValue & predictions$type != "no-recurrence-events")
  
  FPR[i]=nrow(selectedRows2)/totalNegativeNumber
  
  ## Increase the threshold
  startValue=startValue+0.01
  
}


TPR

FPR
#######################

TPR
FPR

plot(x=FPR, y=TPR, xlab = "FPR", ylab = "TPR")
lines(x=seq(0,1, by=0.01), y=seq(0,1, by=0.01) , type="p", col="red")



##########################################################################################
################    ARTIFICIAL NEURAL NETWORKS

library(neuralnet)

####################
mydata = read.table("plantData-Full.csv", header = TRUE, sep = ",")
mydata = read.table("plantData-Full.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

mydata$Species = as.factor(mydata$Species)

for(i in 1:4){
  mydata[,i] = normalize(mydata[,i])
}

str(mydata)

countTraining = round(nrow(mydata)*0.7)
dfTraining = mydata[1:countTraining,]
dfTest = mydata[(countTraining+1):nrow(mydata),]

##
## Set a seed to reproduce your results
set.seed(100)

## Train a neural network with one hidden layer and four neurons
myNN = neuralnet(Species~., data = dfTraining, hidden = c(4), linear.output = FALSE)


#####
plot(myNN)

#################

summary(myNN)

### Make predictions
myPred = predict(myNN, dfTest)

############

mypredClass = apply(myPred, 1, which.max)


######################### EXERCISE 2

####################

mydata = read.table("seedsData.csv", header = TRUE, sep = ",")
for(i in 1:7){
  mydata[,i] = normalize(mydata[,i])
}
mydata$Variety = as.factor(mydata$Variety)

#############

### Data splitting
countTraining = round(nrow(mydata)*0.7)
dfTraining = mydata[1:countTraining,]
dfTest = mydata[(countTraining+1):nrow(mydata),]

######

myNN = neuralnet(Variety~., data = dfTraining, hidden = c(5,2),linear.output = FALSE)
plot(myNN)

###########

myPred = predict(myNN, dfTest)
head(myPred)

###
table(dfTest$Variety, apply(myPred, 1, which.max))

