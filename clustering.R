# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

 install.packages(c("cluster", "rattle","NbClust"))
 library(cluster)
 library(rattle)
 library(NbClust))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

wine_df <- scale(wine[-1]) 


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		                      set.seed(seed)
	                        wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		                      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine_df)

# Exercise 2:
#   * How many clusters does this method suggest?
#   * Why does this method work? What's the intuition behind it?
#   * Look at the code for wssplot() and figure out how it works

# Since there is a drop around 3rd cluster withing groups sum of squares and it drops 
# off after 3, this methods suggests 3 cluster will be a good fit. 

# This method sets a maximum cluster to consider and also sets a random seed. 
# It loops through each cluster to ensure maximum number of iteration. Hence, this method
# seems to be a good approach

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")



# Exercise 3: How many clusters does this method suggest?

# Here again there is a significant peak at cluster 3 when iterated upto 14 clusters. hence the method is suggesting 
# 3 cluster approach

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_df,3, nstart =  25)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

Comp.km <- table(wine$Type,fit.km$cluster)

install.packages("flexclust")
library(flexclust)
randIndex(Comp.km)

# The adjusted Rand index provides a measure of the agreement between two partitions, 
#adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement).
# Agreement between the wine varietal type and the cluster solution is 0.9.
# This is definitely a good clustering


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?


clusplot(wine_df,fit.km$cluster)

