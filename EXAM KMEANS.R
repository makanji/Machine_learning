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
#making my plant data same as my dataset
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
#this tells us about the cluster in respect to each animals 

#### Plot clustered observationsfor Sepal.Length and Sepal.Width
#######################################################
#comparing both with cluster to know of it quality#
#####################################################
par(mfrow=c(2,1))

plot(df2[c(1,2)], col= clusterResult$cluster) #this shows how SL and SW are distributed

plot(df2[c(1,2)], col=df1)


#to see reason why the other cluster was 61 39 50
#this shows that some of the intra cluster are so close

####now using petal lenght and petal width for clustering

plot(df2[c(3,4)], col= clusterResult$cluster) # Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(df2[c(3,4)], col=df1)

### viewing it all together
par(mfcol=c(2,2))
plot(df2[c(1,2)], col= clusterResult$cluster) #this shows how SL and SW are distributed

plot(df2[c(1,2)], col=df1)

plot(df2[c(3,4)], col= clusterResult$cluster) 
plot(df2[c(3,4)], col=df1)


##### Visualization of our clustering results using ggplot
### First create four singel plots and save them in a variable called p1, p2, p3, p4
### Use grid.arrange to arrange the fpur plots as one figure

p1=ggplot(data = df2, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = 	as.factor(clusterResult$cluster)))+
  theme(legend.position="top") +
  scale_color_manual(values=c("red", "green", "blue")) 

p1

# Plot to see how Sepal.Length and Width data points have been distributed originally as per "class" attribute in dataset
p2=ggplot(data = df2, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(col = df1))+
  scale_color_manual(values=c("blue","red", "green"))+
  theme(legend.position="top") 

p2

# Plot to see how Petal.Length and Petal.Width data points have been distributed originally as per "class" attribute in dataset
p3=ggplot(data = df2, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(col = 	as.factor(clusterResult$cluster)))  + 
  scale_color_manual(values=c("blue","red", "green"))+
  theme(legend.position="top")  
p3

# Plot to see how Petal.Length and Petal.Width data points have been distributed originally as per "class" attribute in dataset
p4=ggplot(data = df2, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point(aes(col = df1))+
  scale_color_manual(values=c("green", "blue","red")) +
  theme(legend.position="top")
p4

grid.arrange(p2,p4, p1, p3) 


