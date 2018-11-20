# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust", "dplyr"))
library(cluster)
library(rattle.data)
library(NbClust)
library(dplyr)

# Now load the data and look at the first few rows

data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function

df <- scale(wine[-1])

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

wssplot(df)

# Exercise 2:
#   * How many clusters does this method suggest?
#     This method appears to suggest 3 clusters, as this where the bend in the graph reaches its apex.

#   * Why does this method work? What's the intuition behind it?
#     This method attempst to track the reduction in the within group sum of squares and where the curve
#     bends signifies the point of greatest reduction relative to the number of clusters. The goal is to
#     reduce the withing group sum of squares with the least number of clusters - otherwise the clusters'
#     predictive utility is undermined.

#   * Look at the code for wssplot() and figure out how it works

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")

# Exercise 3: How many clusters does this method suggest?
# Like with the wssplot function this method also suggest 3, with 14 criteria landing on 3, by far the
# largest criteria figure

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(df, 3)
str(fit.km)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

table(wine$Type, fit.km$cluster)

# It looks like the model was highly accurate. When we look at the cross-table of wine$Type vs.
# fit.km$cluster we see that the overall accuracy is strong, with the only disagreement between the two seen in 
# in regards to Type 2 wines, where there was some disagreement between the model and the original wine 
# table.


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
clusplot(wine, fit.km$cluster)