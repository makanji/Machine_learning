####################  DAY 3 MACHINE LEARNING ################
#instal FactoInvestigate
library(FactoInvestigate)

############revision
# Analyse the wine.csv file by applying the kmeans clustering
# step 1: normalize the values
# step 2: identify the optimal number of k with elbow method (wss and for loop)
# step 3: plot the wss values with ggplot
# step 4: apply the k-means clustering with optimal number of k  and plot the cluster results
# step 5: (optinal): you can also use the animation package
# step 6: repeat the analysis using the eclust function.

normalize= function(x){
  normalizedValues= (x-min(x))/(max(x)-min(x)) 
  return (normalizedValues)
}
##normalizing the data first

mydata = read.table("wine.csv", header = TRUE, sep =",")
dim(mydata)
summary(mydata) # to know if theres NA or not
df2 = mydata
df1 # no character
mydata <- df2
for (i in 1:ncol(df2)) {
  df2[, i] = normalize(df2[,i])
  
}

#finding the k using elbow method
wss = c()
for(k in 1:10){
  cluster = kmeans(df2, centers = k)
  wss[k] = sum(cluster$withinss)
}

plotValues = data.frame(k = 1:length(wss), values = wss)
ggplot(data = plotValues, aes(x = k, y = values)) + geom_point() + geom_line() + labs(x = "Number of Clusters", y = "Within groups sum of squares")

#using elbow method we saw our k = 3

clusterResult = kmeans(mydata, centers = 3)
ggplot(data = mydata, aes(x = Alcohol, y = MalicAcid)) + geom_point(color = as.factor(clusterResult$cluster)) + guides(color = "none")
## using eclust package
#we can also find gap statistic with the package

res.km= eclust(mydata, "kmeans") #calculates kmeans und draw plot
fviz_cluster(res.km, data = df2, nstart=50) #plotting our eclcust is visualize using FVIZ_CLUSTER
# Gap statistic plot
fviz_gap_stat(res.km$gap_stat)

############ hierarchical clustering exercise 2 ############
#analysing zoo


mydata = read.table("zoo.csv", header = TRUE, sep =",")
dim(mydata)
summary(mydata) # to know if theres NA or not
##splitting the data
summary(mydata)
df1 = mydata[,c( "animal_name", "type")]
#df1 <- as.factor(df1) # we dont use as factor here bcos its 2 datas and conversion affect its displas as DF
df2 = mydata[,2:17]

#calculation of the distance
distMat = dist(df2)

### Perform the clustering using hclust()

clusterResult = hclust(distMat, method = "average")

### Plot the results with R base plot
plot(clusterResult, labels = df1$animal_name, hang = -1)

### eclust solution
row.names(df2)=df1$animal_name
res.hc <- eclust(df2, "hclust", stand = T) 
### visualization
fviz_dend(res.hc, rect=TRUE)
fviz_dend(res.hc, rect=TRUE,  type="rectangle",horiz = T)
# change the type of plot
fviz_dend(res.hc, rect=TRUE,  type="circular", cex = 0.75)
fviz_dend(res.hc, rect=TRUE,  type="phylogenic")
fviz_dend(res.hc, rect=TRUE,  type="phylogenic", phylo_layout="layout_as_tree")
fviz_dend(res.hc, rect=TRUE,  type="phylogenic", phylo_layout="layout_with_drl")
fviz_dend(res.hc, rect=TRUE,  type="phylogenic", phylo_layout="layout.gem")
fviz_dend(res.hc, rect=TRUE,  type="phylogenic", phylo_layout="layout.mds")
fviz_dend(res.hc, rect=TRUE,  type="phylogenic", phylo_layout="layout_with_lgl")

###### PRINCIPAL COMPONENT ANALYSIS
#used to explain data but doesnt predict the data actually
#help in idetification of important variable and better data understanding


mydata = read.table("plantData-Full.csv", header = TRUE, sep =",")
dim(mydata)
summary(mydata) # to know if theres NA or not
df1 = mydata[,"Species"]
df1 <- as.factor(df1) # we dont use as factor here bcos its 2 datas and conversion affect its displas as DF
df2 = mydata[,c(1,2,3,4)]

for (i in 1:ncol(df2)) {
  df2[, i] = normalize(df2[,i])
  
}

summary(df2)

pca <- prcomp(df2)
?prcomp
summary(pca)
screeplot(pca)
#### plotting using the plot functions  ###
plot(x= df2$Sepal.Length, y= df2$Sepal.Width, col= df1)
plot(pca$x[,1], pca$x[,2], col=as.factor(df1)) #or plot(pca$x[,1], pca$x[,2], col=df1)
pca$rotation #this to know what proportion of each values make various PCA components

### plotting using GGPLOT2  ###
#we havte to create our ploting parameters dataframes
pca.data = data.frame(Species = df1, X = pca$x[,1], Y = pca$x[,2]) #X.Y gives name to the dataFrame made

ggplot(data = pca.data, aes(x = X, y= Y, color = Species)) + geom_point()  + labs(x = "PC1", y="PC2")

screeplot(pca)

pca.var <- pca$sdev^2
pca.var

pca.var.per <- round(pca.var / sum(pca.var) * 100, 1) 

pca.var.per
 
barplot(pca.var.per, xlab = "Principal Component", ylab = "Percent Variation")

ggplot(data = data.frame(Percentage = pca.var.per), 
       aes(x = 1:length(Percentage), y= Percentage))  + geom_col()  +
  labs(x = "Principal Component", y= "Percent Variation")

#################
pca.var = pca$sdev^2 ##this give me eigen values
pca.var

sum(pca.var) #total eigen values 

pca.var.per = round(pca.var / sum(pca.var) * 100, 1)


pca.var.per #this gives variations per PC 

barplot(pca.var.per, xlab = "Principal Component", ylab = "Percent Variation")

ggplot(data = data.frame(Percentage = pca.var.per), 
       aes(x = 1:length(Percentage), y= Percentage))  + geom_col()  +
  labs(x = "Principal Component", y= "Percent Variation")


################################################
#loading scores which is derives through PCA$ROTATION command
#this let us have idea of the parameters that are must important in the PCA

#### Check the variable importance using the loading scores

## For PC1 select the first column
pc1_loading_scores = pca$rotation[,1]
pc1_loading_scores
pc1_attribute_scores <- abs (pc1_loading_scores)


####PC2
pc2_loading_scores = pca$rotation[,2]
pc2_loading_scores



#### PCA with factoExtra and FactoMineR
### Scree plot 
res.pca=prcomp(df2)
fviz_eig(res.pca)

###########
?fviz_pca_ind
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("red", "green", "blue"),
             repel = F     # Avoid text overlapping
)

#########  f
res.pca=prcomp(df2)
fviz_eig(res.pca)

#Graph of individuals. Individuals 
#with a similar profile are grouped together
?fviz_pca_ind
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("red", "green", "blue"),
             repel = F     # Avoid text overlapping
)



### factor extra and factor miner are proffersion lib used for PCA
