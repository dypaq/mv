install.packages("factoextra")
library(factoextra)


library(cluster)

install.packages("NbClust")
library(NbClust)

install.packages("ggplot2")
library(ggplot2)

install.packages("gridExtra")
library(gridExtra)

install.packages("patchwork")
library(patchwork)

install.packages("maps")
library(maps)

install.packages("dplyr")
library(dplyr)

# Read the CSV file (replace 'your_file.csv' with the actual file path)
df <- read.csv("C:/Users/Abhay/Downloads/Sales_Product_Details (1).csv")
head(df, 2)

# Convert non-numerical col to numerical col
df$Product_Description <- as.numeric(factor(df$Product_Description))
df$Product_Line <- as.numeric(factor(df$Product_Line))
df$Product_Category <- as.numeric(factor(df$Product_Category))
df$Raw_Material <- as.numeric(factor(df$Raw_Material))
df$Region <- as.numeric(factor(df$Region))
head(df, 2)

# Check for null values and remove it
df1 <- na.omit(df)
null_count <- colSums(is.na(df1))
print(null_count)
head(df1)

# Standardize the data
df1 <- scale(df1)
head(df1, 2)

# Visualize optimal number of clusters using WCSS
fviz_nbclust(df1, kmeans, method = 'wss')

fviz_nbclust(df, FUN=hcut, method = "silhouette")


# Perform k-means clustering with k = 3 clusters
km1 <- kmeans(df1, centers = 3, nstart = 25)
km1

# Plot results of final k-means model
fviz_cluster(km1, data = df1)

# Find means of each cluster
cluster_means <- aggregate(df, by = list(cluster = km1$cluster), mean)
cluster_means

# Combine cluster assignments with original data
final_data <- cbind(df, cluster = km1$cluster)

# Function to create bar graphs for Product_ID against Quantity for all clusters together
plot_product_id_vs_quantity_all_clusters <- function(cluster_data) {
  ggplot(cluster_data, aes(x = as.factor(Product_ID), y = Quantity)) +
    geom_bar(stat = 'summary', fun = 'mean', fill = 'skyblue', color = 'black') +
    labs(title = 'Product ID vs. Quantity',
         x = 'Product ID', y = 'Average Quantity') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
}

# Create bar graph for all clusters together
plot_list <- list()
for (i in unique(final_data$cluster)) {
  cluster_subset <- final_data[final_data$cluster == i, ]
  plot_list[[i]] <- plot_product_id_vs_quantity_all_clusters(cluster_subset) +
    ggtitle(paste('Cluster', i))
}

# Combine plots into a single plot with subplots
all_plots <- do.call(grid.arrange, c(plot_list, ncol = 3))

# Display the combined plot
print(all_plots)

# Create pie charts for each cluster
plots <- lapply(unique(final_data$cluster), function(cluster_num) {
  cluster_subset <- final_data[final_data$cluster == cluster_num, ]
  region_counts <- table(cluster_subset$Region)
  pie_data <- data.frame(region = names(region_counts), count = as.numeric(region_counts))
  ggplot(pie_data, aes(x = '', y = count, fill = region)) +
    geom_bar(width = 1, stat = 'identity') +
    coord_polar(theta = 'y') +
    labs(title = paste('Region Distribution for Cluster', cluster_num)) +
    theme_void() +
    theme(legend.position = 'right')
})

# Combine plots into a single plot with subplots
combined_plot <- wrap_plots(plots, nrow = 1)

# Display the combined plot
print(combined_plot)

# Add cluster information to the dataset
df$Cluster <- as.factor(km1$cluster)


# Group points with the same coordinates and cluster, and print corresponding dates
grouped_df <- df %>%
  group_by(Latitude, Longitude, Cluster) %>%
  summarise(Dates = paste(Date, collapse = ', '))

print(grouped_df)


# Compute the distance matrix using Euclidean distance
res.dist <- dist(df)

# Perform hierarchical clustering
res.hc <- hclust(d = res.dist, method = 'ward.D2')

# Print the hierarchical clustering result
print(res.hc)

# Visualize the dendrogram with cluster membership
fviz_dend(res.hc, cex = 0.5, k = 3, color_labels = TRUE)

# Compute cophenetic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and the original distance
correlation <- cor(res.dist, res.coph)
print(correlation)

# Perform hierarchical clustering with a different method (optional)
res.hc2 <- hclust(res.dist, method = 'average')
correlation2 <- cor(res.dist, cophenetic(res.hc2))
print(correlation2)

# Visualize the dendrogram with cluster membership by average method
fviz_dend(res.hc2, cex = 0.5, k = 3, color_labels = TRUE)

# Determine the cluster membership
grp <- cutree(res.hc, k = 3)
print(grp)

# Display the distribution of clusters
table_grp <- table(grp)
print(table_grp)


