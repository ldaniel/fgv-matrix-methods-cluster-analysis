# Loading required libraries
library(readxl)
library(dplyr)
library(ggcorrplot)
library(fpc)
library(factoextra)

# reading the dataset
ATIBAIA <- read_xlsx("./data/raw/ATIBAIA.xlsx", 
                     sheet = "ATIBAIA", 
                     col_names = TRUE)

# preparing dataset for cluster algorithim
ATIBAIA <- select(ATIBAIA, c(instal, biling, estac, train, ti, social))
ATIBAIA <- mutate_all(ATIBAIA, as.numeric)
ATIBAIA <- lapply(ATIBAIA, scale) %>% as_tibble

# preparing corelogram
ggcorrplot(cor(ATIBAIA),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method="square",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlation Matrix")

# function prepared to find the best k for cluster analysis
# this function will return a tibble containing the k, withinss, betweenss 
# and the absolute diferrence of withinss and betweenss
# k with the minimun diferrence is the best compromise between withinss and betweenss metrics
# we will use that k as the best number of clusters

find_best_k <- function(x, k_limit, nstart) {

  metrics <- tibble()
  
  for (k in seq(1, k_limit, 1)) {
    temp <- kmeans(x, k, nstart = nstart)
    df_temp <- tibble(k = k,
                      withinss = temp$tot.withinss,
                      betweenss = temp$betweenss,
                      best_fit = abs(withinss - betweenss))
    metrics <- bind_rows(metrics, df_temp)
  }

  return(metrics)
  
}

# calling the custom function
best_k <- find_best_k(ATIBAIA, 29, nstart = 20)

# ploting withinss and betweenss graph and its best k
ggplot(data = best_k, aes(x = k)) +
  geom_point(aes(y = withinss), color = 'blue') +
  geom_point(aes(y = betweenss), color = 'red') +
  geom_line(aes(y = withinss), color = 'blue') +
  geom_line(aes(y = betweenss), color = 'red') +
  geom_segment(aes(x = which.min(best_k$best_fit), 
                   y = best_k$withinss[which.min(best_k$best_fit)], 
                   xend = which.min(best_k$best_fit), 
                   yend = best_k$betweenss[which.min(best_k$best_fit)]),
               linetype = "dashed", color = "black") +
  geom_label(aes(x = which.min(best_k$best_fit), 
                 y = best_k$withinss[which.min(best_k$best_fit)], 
                 label = paste('best k is ', which.min(best_k$best_fit))
                 )
             )

# calculating kmeans based on the best k found above
KMeans_clustering <- kmeans(ATIBAIA, which.min(best_k$best_fit), nstart = 20)

# Visualizing clusters
plotcluster(ATIBAIA, KMeans_clustering$cluster)
fviz_cluster(list(data = ATIBAIA, cluster = KMeans_clustering$cluster),
             show.clust.cent = T)
