#Cleaning The Data and Statistics Descriptive
#First of all call the data to be used
data("USArrests")
install.packages("MVN")
library(MVN)
outlier=mvn(USArrests,multivariateOutlierMethod = 
              "quan",showNewData = TRUE)
#Based on the plot above, there are 8 outliers in the data. But I’ll just discard the 
#Alaska data because it’s so far away from the other observations.
df = USArrests[-2,]
df =scale(USArrests)

#Hierarchical Clustering
#Before doing clustering, look for the optimal k value first by using the 
#Silhouette method.

library(factoextra)
fviz_nbclust(df, FUN=hcut, method = "silhouette")

metode_al=hclust(dist(df),method="average")
hc_ave=cophenetic(metode_al)
cor.ave=cor(as.dist(dist(df)),hc_ave)

metode_sl=hclust(dist(df),method="single")
hc_single=cophenetic(metode_sl)
cor.sl=cor(as.dist(dist(df)),hc_single)

metode_cl=hclust(dist(df),method="complete")
hc_cl=cophenetic(metode_cl)
cor.cl=cor(as.dist(dist(df)),hc_cl)

metode_w=hclust(dist(df),method="ward.D")
hc_w=cophenetic(metode_w)
cor.w=cor(as.dist(dist(df)),hc_w)

metode_cd=hclust(dist(df),method="centroid")
hc_cd=cophenetic(metode_cd)
cor.cd=cor(as.dist(dist(df)),hc_cd)

cbind.data.frame(cor.ave,cor.w,cor.cl,cor.single,cor.cd)


plot(metode_al)
rect.hclust(metode_al,2)

km <- kmeans(df, centers = 2, nstart = 25)
fviz_cluster(km, data = df)

#Cluster Profiling
group2=cutree(metode_al,2)
table(group2)
table(group2/nrow(df))
aggregate(df,list(group2),mean)

