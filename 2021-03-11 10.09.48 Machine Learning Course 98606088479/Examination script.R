#### clustering ### day 2######

mydata <- read.table("plantData-Full.csv", header = T, sep = ",", stringsAsFactors = T)


dim(mydata)
summary(mydata)


######splitting out data set to differentiate categorical data from numerical values

df1=mydata[, ]


df2=mydata[, c(1,2,3,4)]

#df2 <- subset(mydata, select = c(-Species))

summary(df3)
summary(df2)

########### kmeans animation for better understanding##########

#this to show the idea of the cluster center and grouping done from the points
kmeans.ani (x = df2, centers = 3,
            hints = c("cluster centre moving", "finding cluster"),
            pch = 2:4, col = 2:4)
#################################################################################
                #normalization of the vector function
normalize= function(x){
  normalizedValues= (x-min(x))/(max(x)-min(x)) 
  return (normalizedValues)
}

#normalization of df2 values using my function
 
for (i in 1:ncol(df2)) {
  df2[, i] = normalize(df2[,i])
  
}


### Perfomr k-means clustering for df2 with 3 cluster
#also make sure d data contain number only
clusterResult = kmeans(df2,centers=3) #apply k-means algorithm with k=3

clusterResult$size #tells us how many onjects we have in each cluster

clusterResult$cluster 
#this tells us about the cluster in respect to each animals 

#### Plot clustered observationsfor Sepal.Length and Sepal.Width
par(mfrow=c(2,1))

plot(df2[c(1,2)], col= clusterResult$cluster) #this shows how SL and SW are distributed

plot(df2[c(1,2)], col=df1)

summary(mydata$Sepal.Width)
#to see reason why the other cluster was 61 39 50
#this shows that some of the intra cluster are so close

#######################################################
#comparing both with cluster to know of it quality#
#####################################################
par(mfrow=c(2,1))
plot(df2[c(1,2)], col= clusterResult$cluster) #this shows how SL and SW are distributed

plot(df2[c(1,2)], col=df1)

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
######

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

grid.arrange(p2,p1, p4, p3) 
grid.arrange(p4,p3) 

### finding the k using elbow method other mtd is GAP STATISTICS
 
wss <- c() #within cluster SSQ

for (i in 1:20) {
  cluster <- kmeans(df2, centers = i)
  wss[i]=sum(cluster$withinss)
  #sum of withincluster sum of squares 
  cat("i=", i, ";", i,". cluster wss score=", wss[i], "\n")
}

#ploting the wss values using plot function and ggplot2
#from the plot seeing the elbow changing plot we can determine what k number suite more

#### This plot is called elbow-method
par( mfrow = c(1,1) )

plot(1:length(wss), wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

##ggplot 
plotValues=data.frame(k=1:length(wss), values=wss)
p = ggplot(plotValues, aes(x=k, y=values))
p
p = p + geom_point() +
geom_line()
p

####### fator extra package###################
#a very powerful package for clustering analyis
#analysis + visualization
#function: eclust===>enhanced cluster analysis

library(factoextra) #makes prof analysis
?eclust # tells us about how to use it; KMEANS "eclust"

res.km <- eclust(df2, "kmeans" ) #calculate kmeans and draw the plot
#used multi dimensional scaling to draw the plot of the four variables which are been scales to fitables dimensions
#total dimension shows how many variation that could be explainable using the cluster


clusterResult= kmeans(df2,centers=3)


fviz_cluster(clusterResult, data = df2)

 # you can add some parameters to your plot 
fviz_cluster(clusterResult, data = df2, geom = "point",
             frame.type = "norm") + theme_bw()

# You can use elbow or gap statistic method
# to identify the optimal number of clusters

fviz_nbclust(df2, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) + labs(subtitle = "Elbow method")

# Gap statistic plot
fviz_gap_stat(res.km$gap_stat) #small data usage majorly 
                                #to find optimum cluster number
                                #doesnt require any effort and its a succesful mtd 


##################################################################
####################### hierarchical cluster  ####################
##################################################################


mydata <- read.table("animalData.csv", header = T, sep = ",")


dim(mydata)
View(mydata)
dim(mydata)
summary(mydata)

#splitting thr data

par(mfrow= c(1,1))
df1 <- mydata[,"animal"]
df2 <- subset(mydata, select =-c(animal))#or mydata[,1:85]

### Data splitting
df1=as.factor( mydata[, "animal"])

df2= subset(mydata, select=-c(animal))  
### Compute the distance matrix

distMat = dist(df2)

#perfrom dendogram using the calculated distance

### Perform hierarchical clustering
clusterResult= hclust(distMat, method = "average")

plot(clusterResult)
plot(clusterResult, hang = -1) #not proffesional hang makes it cocmes to thesame level

#install.packages("ggdendro")
library(ggdendro)
ggdendrogram(clusterResult) #making more professional plot thts better

#For adding labels we need an extra step at the beginning before the clustering
#adding the names to the df2 and making all our analysis to reflect the name 
rownames(df2) = df1 #We assign the animal names as row names to df2 
distMat = dist(df2)
clusterResult = hclust(distMat, method = "average")
#?hclust
ggdendrogram(clusterResult)

ggdendrogram(clusterResult, rotate = T) #rotating plot

##phylogenic tree #which also require the packeage called ape
library("ape")

plot(as.phylo(hc), cex = 0.7, label.offset = 0.1) #cex <- font size, label.offset <- spaning of label (like hang: -1)

plot(as.phylo(hc), type = "cladogram",  cex = 0.7, label.offset = 0.1 )
plot(as.phylo(hc), type = "unrooted",  cex = 0.7, label.offset = 0.1 )
plot(as.phylo(hc), type = "fan",  cex = 0.7, label.offset = 0.1 )

###Enhanced hiererchical clustering
res.hc <- eclust(df2, "hclust", stand = T) #this compute hclust

fviz_dend(res.hc, rect=TRUE)


fviz_dend(res.hc, rect=TRUE,  type="rectangle",horiz = T) #rotate the plot
# change the type of plot 
fviz_dend(res.hc, rect=TRUE,  type="circular", cex = 0.75)

fviz_dend(res.hc, rect=TRUE,  type="phylogenic") #phylogeny tree


library(igraph)

###########################exercise solution###################

mydata = read.table("seedsData.csv", header = TRUE, sep =",")

### Check the dimension and summary statistics
dim(mydata)
summary(mydata)

### Split the data

df2 = mydata[, 1:7]
df1 = mydata[, "Variety"]

### Normalize the data
for(i in 1:7){
  df2[,i] = normalize(df2[,i])
}
#### Perform the kmeans analysis

wss = c()
for(k in 1:10){
  cluster = kmeans(df2, centers = k)
  wss[k] = sum(cluster$withinss)
}

### Create a dataframe to use ggplot 
plotValues = data.frame(k = 1:length(wss), values = wss) #making our values of k and WCSQ
#From Manuel Goldkuhle to Everyone:  03:04 PM

ggplot(data = plotValues, aes(x = k, y = values)) + 
  geom_point() +   geom_line()  +
  labs(x = "Number of Clusters", y = "Within groups sum of squares")

##we can also visualise more using thr animation function to visualize our data more
df3 <- df2[,c(3,4)]
kmeans.ani (x = df3, centers = 3,
            hints = c("cluster centre moving", "finding cluster"),
            pch = 2:4, col = 2:4)

# k=3
clusterResult = kmeans(df2, centers = 3)

x=ggplot(data = df2, aes(x = Area)) + 
  geom_histogram( aes(fill = as.factor(df1)), bins = 30) + guides( fill = "none")


y=ggplot(data = df2, aes(x = Area)) +
  geom_histogram( aes(fill = as.factor( clusterResult$cluster )), bins = 30) + guides(color = "none")

grid.arrange(x,y)

a=ggplot(data = df2, aes(x = Length_Kernel, y= Length_Groove)) + 
  geom_point(color = as.factor(df1)) + guides(color = "none")


b=ggplot(data = df2, aes(x = Length_Kernel, y= Length_Groove)) +
  geom_point(color = as.factor(clusterResult$cluster)) + guides(color = "none")

grid.arrange(a,b)

### Analysis using eclust-package

res.km= eclust(df2, "kmeans") #calculates kmeans and draw plot
fviz_cluster(res.km, data = df2, nstart=50) #actualy didnt see any difference without using nstart
?fviz_cluster

# Gap statistics
fviz_gap_stat(res.km$gap_stat) # to find k

####### hiererchical clustering exercise######################

mydata = read.table("plantData-Full.csv", header = TRUE, sep =",")
dim(mydata)

summary(mydata)
df1 = as.factor( mydata[, "Species"])
df1
#renaming the df1 and differentiating it for easy reflection on the dendogram

df1 = paste(df1, 1:150, sep = "_") #this was used by DR. mehmet
df1
df2 = mydata[, 1:4]

row.names(df2) <- df1
#calculation of distance
distMat <- dist(df2)


clusterResult = hclust(distMat, method = "average")
plot(clusterResult, hang = -1)
###repeating same analysis using eclust to present it more professionally


res.hc <- eclust(df2, "hclust", stand = T, k=3) # compute hclust
fviz_dend(res.hc, rect=TRUE)

########more visualization
fviz_dend(res.hc, rect=TRUE,  type="rectangle",horiz = T) #rotate the dendogram through HORIZ

fviz_dend(res.hc, rect=TRUE,  type="circular", cex = 0.75)

fviz_dend(res.hc, rect=TRUE,  type="phylogenic")

fviz_dend(res.hc, rect=TRUE,  type="phylogenic", phylo_layout="layout_as_tree")


fviz_dend(res.hc, rect=TRUE,  type="fan", cex = 0.75)