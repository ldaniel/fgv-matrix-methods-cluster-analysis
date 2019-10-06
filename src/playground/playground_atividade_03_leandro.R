# loading required libraries --------------------------------------------------

#libraries for file and data manipulation
library(readxl)
library(dplyr)

# cluster analysis
library(fpc)
library(factoextra)
library(cluster)
library(ggcorrplot)

# data injestion --------------------------------------------------------------
ATIBAIA <- read_xlsx("./data/raw/ATIBAIA.xlsx", 
                     sheet = "ATIBAIA", 
                     col_names = TRUE)

# checking the class for each column and get a summary
sapply(ATIBAIA, class)
summary(ATIBAIA)

# dataset preparation ---------------------------------------------------------

# changing vars biling, estac and ti from numeric to factor
ATIBAIA$biling = as.factor(ATIBAIA$biling)
ATIBAIA$estac  = as.factor(ATIBAIA$estac)
ATIBAIA$ti     = as.factor(ATIBAIA$ti)

# getting only meaningful vars as clustering drivers:
ATIBAIA_drivers = ATIBAIA[, -c(1,2,9)]

# transforming and scaling relevant vars to get the final dataset
ATIBAIA_drivers_num <- ATIBAIA_drivers
ATIBAIA_drivers_num$biling = as.numeric(ATIBAIA_drivers_num$biling)
ATIBAIA_drivers_num$estac = as.numeric(ATIBAIA_drivers_num$estac)
ATIBAIA_drivers_num$ti = as.numeric(ATIBAIA_drivers_num$ti)

ATIBAIA_drivers_num_z <- as.data.frame(lapply(ATIBAIA_drivers_num, scale))

# calculating vars correlation
ggcorrplot(cor(ATIBAIA_drivers_num_z),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method = "square",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Correlation Matrix")

# withinss and betweenss according to its K ----------------------------------

max_number_of_clusters <- nrow(ATIBAIA_drivers_num_z) - 1

results <- tibble(k = numeric(), 
                  withinss = numeric(), 
                  betweenss = numeric(), 
                  best_fit = numeric())

for (count in seq(1, max_number_of_clusters, by = 1)) {
  number_of_clusters <- as.numeric(count)
  KMeans_clustering <- kmeans(ATIBAIA_drivers_num_z, number_of_clusters, nstart = 20)

  results <- add_row(results,
                     k = number_of_clusters,
                     withinss = sum(KMeans_clustering$withinss),
                     betweenss = KMeans_clustering$betweenss,
                     best_fit = abs(sum(KMeans_clustering$withinss) - KMeans_clustering$betweenss))
}

ggplot(data = results, aes(x = k)) +
  geom_point(aes(y = withinss), color = 'blue') +
  geom_point(aes(y = betweenss), color = 'red') +
  geom_line(aes(y = withinss), color = 'blue') +
  geom_line(aes(y = betweenss), color = 'red') +
  geom_segment(aes(x = which.min(results$best_fit), 
                   y = results$withinss[which.min(results$best_fit)], 
                   xend = which.min(results$best_fit), 
                   yend = results$betweenss[which.min(results$best_fit)]),
               linetype = "dashed", color = "black") +
  geom_label(aes(x = which.min(results$best_fit), 
                 y = results$withinss[which.min(results$best_fit)], 
                 label = paste('best k is ', which.min(results$best_fit))))

# choosing the K that best fits -----------------------------------------------
KMeans_clustering <- kmeans(ATIBAIA_drivers_num_z, which.min(results$best_fit), nstart = 20)

# Visualizing clusters
plotcluster(ATIBAIA_drivers_num_z, KMeans_clustering$cluster)
fviz_cluster(list(data = ATIBAIA_drivers_num_z, 
                  cluster = KMeans_clustering$cluster),
             show.clust.cent = T)