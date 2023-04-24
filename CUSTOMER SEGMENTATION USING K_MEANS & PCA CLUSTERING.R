library(cluster)
library(readr)
library(tidyverse)
mall_customers <- read.csv("C:/Users/JIT/Downloads/PYTHON TUTORIALS/Mall_Customers.csv") %>% 
    rename(income=Annual.Income..k.., spending=Spending.Score..1.100.)
colnames(mall_customers)
# mall_data <- as.matrix(mall_customers[, 3:5])
# Descriptive Analysis
# creating histogram to show  dispersion of mall customers based on age 
ggplot(mall_customers, aes(Age)) +
                geom_histogram( ) +
                ggtitle("Histogram showing age distribution")

# creating barplot to assess gender distribution 
ggplot(mall_customers, aes(Gender)) +
         geom_bar(stat = "count", width = 0.5, fill="steelblue") +
         theme_minimal() +
         ggtitle("Bar plot to show gender distribution")

 # create histogram for variable "age" by "gender"   
ggplot(mall_customers, aes(x=Age, fill=Gender, color=Gender)) +
       geom_histogram(bins = 10, alpha=0.5, position = "identity") +
       ggtitle("Histogram for variable age by Gender")

# creating density plot to show customers annual income    

ggplot(mall_customers, aes(income)) +
        geom_density(alpha=.4, fill="orange") +
        scale_x_continuous(breaks = seq(0,200,10)) +
       ggtitle("Density plot to show customers' income")
# creating boxplot to understand customers spending by gender 

ggplot(mall_customers, aes(spending, Gender)) +
 geom_boxplot(alpha=0.5, color="blue") +
    ggtitle("Boxplot showing customers spending by Gender")

# CUNDUCTING CLUSTER ANALYSIS  
# setting up seed for reproducibility   
set.seed(125)
# Using Gap_statistics to get optimal number of clusters   
stat_gap <- clusGap(mall_customers[,3:5], 
                    FUNcluster =kmeans,
                    nstart=25, K.max = 10, B = 50)

# Plot the optimal number of clusters    
plot(stat_gap)     # based on gap_statistics 6 is the optimal number of clusters 

# Creating the clusters with Kmeans    
k6 <- kmeans(mall_customers[,3:5], 6, iter.max = 100, nstart = 50,
             algorithm = "Lloyd")
k6
# Showing the kmean  clusters 
clusplot(mall_customers, k6$cluster, 
         color=T, shade=T,
         labels = 0, lines = 0)


# NEXT WE PERFORM PRINCIPAL COMPONENT ANALYSIS 
# TO REDUCE DIMENSIONALITY OF THE DATA AND CAPTURE THE TWO MOST SIGNIFIGANT COMPONET

# Perform PCA Analysis 
pcclust <- prcomp(mall_customers[,3:5],scale. = FALSE)
summary(pcclust)
pcclust$rotation[,1:2]


# Plot the segments based on the results from cluster analysis and PCA    

set.seed(1)
# Create plot for customers segments 

ggplot(mall_customers, aes(income, spending)) +
       geom_point(stat = "identity", aes(color=as.factor(k6$cluster))) +
     scale_color_discrete(name = " ", 
                        breaks = c("1","2","3", "4","5","6"), 
                        labels = c("High-Income, Low Spending ", "Low Income, Low spending",
                                   "Medium Income , Medium Spending",
                                   "Medium Income , Medium Spending",
                                   "Low Income , High Spending","High Income , High Spending")) +
 ggtitle("Segments of mall_customers", subtitle = "Using K-means clustering ")


