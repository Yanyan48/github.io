### Analysis the relationship among Microbiome Data ###

## Part 1:
# The purpose of this script is to find the relationship among microbes (OTU) 
# and the sample clustering by using a Gaussian graphical model and cluster analysis.
# Use a Gaussian graphical model to construct a network for the first 10 microbes 
# (OTUs) in the data set.

# # # # # # # # # # # # # # # # # # # # #
# Install required packages
# ppcor for calculating partial correlations along with p-value
# igraph for generating graphs
# # # # # # # # # # # # # # # # # # # # #

library(ppcor) 
library(igraph)

# Loading the tobacco data set
load('/Users/anna/Desktop/ASU/Fall 22/LSC 598 II - Yue Wang/STP 560 Lectures/Module IV/tobacco_clr.Rdata')

# Use the first 10 microbes (OTUs) in the data set(the first 10 columns of the data matrix)
data10 <- tobacco_clr$data[,1:10]

# Visualize the network
network <- (pcor(data10)$p.value <= 0.05)*1 # use alpha = 0.05 test
diag(network) <- 0 # remove self loops
network.plot <- graph_from_adjacency_matrix(network)
plot(network.plot)

## CONCLUSION:
# The network graph depicts 10 microbes as nodes and their conditional independence 
# as edges between them under the Gaussian Graphical Model. The entries of the partial 
# correlation matrix calculated by pcor() function give the skeleton of the network. 
# Since variables we constructed are approximately normally distributed, we can estimate 
# the results under the 0.05 significance level. 181589 and 255657 have an edge connecting 
# them , so they are conditionally dependent. In addition, there are many other nodes 
# with edges that are estimated to be conditionally dependent such as 164413 and 176468, 
# 176468 and 4425571, and 192795 and 219151 etc.

## Part 2: 
# The purpose of this script is use cluster analysis to integrate different 
# data sources, through analyzing the different genetic features of different phenotypes 
# in order to determine commonalities, these commonalities can help us understand the 
# evolutionary relationship among the microbes in the data set.


# Perform a PCA-based clustering analysis for the samples based on the data set.

# Loading the tobacco data set
load("/Users/anna/Desktop/ASU/Fall 22/LSC 598 II - Yue Wang/STP 560 Lectures/Module IV/tobacco_clr.Rdata")

data_c <- scale(tobacco_clr$data[,1:271], center = T, scale=F)
# Breaks a matrix into three other matrix
svd.data <- svd(data_c)
svd.left <- svd.data$u
svd.right <- svd.data$v
svd.values <- svd.data$d
PC1 <- svd.values[1]*svd.left[,1]
PC2 <- svd.values[2]*svd.left[,2]
# Plot the data
plot(PC1, PC2, xlab = 'PC 1',ylab = 'PC 2', col = tobacco_clr$sample.color, pch = tobacco_clr$sample.pch)

## CONCLUSION:
# The graph shows the separation of the samples. There are 45 samples and each 
# sample has their own color and pch symbols.
# From the graph, we can see that there are three groups of samples with different 
# colors (orange, purple and blue) that are separated from each other. The orange 
# and purple are more clustered while the blue ones are more scattered.
# If we visualize the graph based on the pch symbols, we can see that the triangle 
# points down and plus are well separated with other symbols, but it is hard to 
# distinguish other symbols with each other. In conclusion, the sample clustered 
# better based on the sample color, and well separated based on some special pch symbols.

# Perform the CoIA on "tobacco_clr$X" and "tobacco_clr$H" and construct a CoIA-plot 
# to show the sample clustering of the observations. 

X <- tobacco_clr$data
Y <- tobacco_clr$H
CoIA <- svd(t(X)%*%Y)
CoIA.X1 <- X%*%CoIA$u[,1]
CoIA.Y1 <- Y%*%CoIA$v[,1]
plot(CoIA.X1, CoIA.Y1, col = tobacco_clr$sample.color, pch = tobacco_clr$sample.pch)

## CONCLUSION:
#  From the graph, we can see that the samples are not well clustered based on the color, 
# the orange and the purple are a little bit separated from the other two colors whereas 
# the blue one is not clustered at all. However, we can separate parts of the sample better 
# when we use the pch symbols, especially in blue samples. For example, the triangle point 
# down, circle, square cross ,and diamond are well clustered based on the pch symbols.


# Compare the plots constructed in Steps 1 and 2.
# Similarities: The two steps both use Singular Value Decomposition(SVD)function 
# to cluster samples in graphs. They both separate samples with some features through 
# colors and pch symbols in the graphs.
# Differences: The step 1 PCA-based clustering analysis performed one data set while 
# step 2 performed the CoIA with two data sets. The samples are better separeted by 
# the color in step 1 than in step 2, while some samples are better separated by pch 
# symbols in step 2 than in step1.


# The end!
#=================================================
