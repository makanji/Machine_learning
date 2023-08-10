#################################################################################################
###########################    K MEAN CLUSTERING   ##############################################
#################################################################################################
# Basically we clasifyour datas base on their elements of similaries
#datas are cleaned to ensure no NA, Character are cheked to ensure the are in factors 
#Datasets are also been normalized 
#Kmeans Clustering is applied through factor extra libry or inbuilt functions
#the cluster is also compare with our normal data sets to validate the efective result of the cluster anaylsis a
#sample size are determine through elbow methods or gapit statistics



#importation of datasetss

plantData <- read.table(file = "plantData.csv", header = T, sep =",", dec = "." )

#cleaning up data sets 

dim(plantData)
summary(plantData)

#replacemensts of NA using means values


Mean_SepalL= round( mean(plantData$Sepal.Length, na.rm = TRUE), 1) # rounding the value to 1DP

mean_SepalW=round( mean(plantData$Sepal.Width, na.rm = TRUE),1)

#replacing datas NA with mean values

plantData$Sepal.Length[is.na(plantData$Sepal.Length)]=  Mean_SepalL


plantData$Sepal.Width[is.na(plantData$Sepal.Width)] =  mean_SepalW
summary(plantData)

### checking out the characters to factors 
plantData$Species <- as.factor(plantData$Species)

######splitting out data set to differentiate categorical data from numerical values

mydata <- plantData

df1=mydata[,"Species" ]

df2 <- subset(mydata, select = c(-Species))


########### kmeans animation for better understanding##########

#this to show the idea of the cluster center and grouping done from the points
kmeans.ani (x = df2, centers = 3,
            hints = c("cluster centre moving", "finding cluster"),
            pch = 2:4, col = 2:4)

####Normalization of dataset 

normalize= function(x){
  normalizedValues= (x-min(x))/(max(x)-min(x)) 
  return (normalizedValues)
}

#normalization of df2 values using my function

for (i in 1:ncol(df2)) {
  df2[, i] = normalize(df2[,i])
  
}

####### fator extra package###################
#a very powerful package for clustering analyis
#analysis + visualization
#function: eclust===>enhanced cluster analysis

library(factoextra) #makes prof analysis

?eclust # tells us about how to use it; KMEANS "eclust"

res.km <- eclust(df2, "kmeans" ) #calculate kmeans and draw the plot, this automatic

clusterResult= kmeans(df2,centers=3)


fviz_cluster(clusterResult, data = df2)

# You can use elbow or gap statistic method
# to identify the optimal number of clusters

fviz_nbclust(df2, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "Elbow method")
  #tell r to add vert lines in the elbow location   

# Gap statistic plot
fviz_gap_stat(res.km$gap_stat) #small data usage majorly 
#to find optimum cluster number
#doesnt require any effort and its a succesful mtd 

### Perfomr k-means clustering for df2 with 3 cluster
#also make sure d data contain number only
clusterResult = kmeans(df2,centers=3) #apply k-means algorithm with k=3

clusterResult$size #tells us how many onjects we have in each cluster

clusterResult$cluster 

###################################################################################################

mydata <- read.table("catsData.csv", header = T, sep = ",", stringsAsFactors = T)
dim(mydata)
summary(mydata)

#### after this we have to split our data, make out 
### Take random samples for the training and test set

# Define 70% as training data and 30% as test data (these numbers are set manually)

countTraining <- round(nrow(mydata)*0.7) #we make 70 percent

randomRows=sample(1:nrow(mydata), size= countTraining, replace=F)

### Take random samples for the training and test set
# Select random rows and subset the data
#taking randon dataset for the 70percents
summary(randomRows)
length(randomRows)

dfTraining = mydata[randomRows,]

dfTest = mydata[-randomRows,]

# Get an overview of the training data
summary(dfTraining)
summary(dfTest)
####
#sex in your data should be set as factor

#install.packages("e1071")

#### Be aware: depending on your R version you have to defien Sex as a factor
#define
### Apply SVM 
# Here, we are starting with a linear kernel 

mySVM = svm(as.factor(Sex)~., data = dfTraining, kernel="linear")
summary(mySVM)

# parameters are optimize using the tune function to acquire best performance datasets
#best püerforming parameters are now used to optimize our SVM


################plotting SVM CLASSIFIER

plot(mySVM, data = dfTraining)

plot(mySVM, dfTraining,  svSymbol = "+", dataSymbol = "o")

# crosses are support vectors while circles are normal data points
#on the plot we dont see the hyperplane but we can only see support vectors that are closer to the hyperplanes

#prediction using our SVM model and testing on our DfTEst data sets
## Create a confusion matrix to check the level of our predictions
mypred <- predict(mySVM, dfTest)

confTable <- table(mypred, as.factor( dfTest$Sex))
confTable #comparism of the predicton agaisnt actual values

#rows are the predictions
#columns are the actual ones that is the reality 

################################################################## 
#true_positiv = sum(diag(confTable))
#all_preds = sum(confTable)
    #pred_perf = true_positiv / all_preds               #the case of not 2X2 matrix
#############################################################

truePos = confTable[1,1] 
falsePos = confTable[1,2]
falseNeg = confTable[2,1]
trueNeg = confTable[2,2]

### with these numbers we can calculate different metrics to access our model performance
### Task: write four different functions to calculate: the sensitivity, specificity, accuracy and precision
### Use the number of true positives, true negatives, false positives and false negatives as input

# Defining a funtion to that calculate SEN,SPEC,ACC, PREC to know how effective our model is
## the sensitivity
## the specificity
## the precision
## teh accuracy

calcSens = function(truePos,falseNeg){
  sens = truePos / (truePos + falseNeg)
  return(sens)
}
calcSpec = function(trueNeg,falsePos){
  spec = trueNeg / (trueNeg + falsePos)
  return(spec)
}
calcPrec = function(truePos,falsePos){
  prec = truePos / (truePos + falsePos)
  return(prec)
}
calcAcc = function(truePos,trueNeg,falsePos,falseNeg){
  acc = (truePos + trueNeg) / (truePos + trueNeg + falsePos + falseNeg)
  return(acc)
}

##########################
### Call the functions with the values obtained from the confusion matrix
sens = calcSens(truePos,falseNeg)
spec = calcSpec(trueNeg,falsePos)
prec = calcPrec(truePos,falsePos)
acc = calcAcc(truePos,trueNeg,falsePos,falseNeg)

sens
spec
prec
acc
