library(readxl)
library(dplyr)
library(ggcorrplot)
library(fpc)
library(factoextra)
library(reshape2)

df <- read_xlsx('data/6-metodos-matriciais-e-analise-de-cluster/base_sanduiches.xlsx')

# preparing dataset for cluster algorithim ----
sapply(df, class) %>% as.data.frame
df <- select(df, -c(Sanduíches))
df <- mutate_all(df, as.numeric)
df <- lapply(df, scale) %>% as_tibble

# preparing corelogram ----
ggcorrplot(cor(df),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method="square",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlation Matrix")

# function prepared to find the best k ----
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

best_k <- find_best_k(df, 19, nstart = 20)

# ploting withinss and betweenss graph and its best k ----
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
  geom_label(aes(x = which.min(best_k$best_fit) + 2, 
                 y = best_k$withinss[which.min(best_k$best_fit)] + 4, 
                 label = paste('best k is ', which.min(best_k$best_fit))
  ))

# number os clusters best fit ----
## calculating kmeans based on the best k found above
KMeans_clustering <- kmeans(df, which.min(best_k$best_fit), nstart = 20)
KMeans_clustering <- kmeans(df, 5, nstart = 20)

## Visualizing clusters
plotcluster(df, KMeans_clustering$cluster)
fviz_cluster(list(data = df, cluster = KMeans_clustering$cluster),
             show.clust.cent = T)

cluster_id_1 <- KMeans_clustering$cluster

# adding cluster information to the original data ----
df <- read_xlsx('data/6-metodos-matriciais-e-analise-de-cluster/base_sanduiches.xlsx')

# preparing dataset for cluster algorithim ----
sapply(df, class) %>% as.data.frame
df <- select(df, -c(Sanduíches))
df <- mutate_all(df, as.numeric)
df <- lapply(df, scale) %>% as_tibble

# trying to remove high correlated variables ----
remove_high_cor <- function(cor_mtx, correl_threshold = 0.75) {
  
  reject_variables_vector <- tibble(var_1 = row.names(cor_mtx)) %>% 
    bind_cols(as_tibble(cor_mtx)) %>% 
    melt(id = c("var_1")) %>% 
    filter(var_1 != variable) %>%
    mutate(abs_value = abs(value)) %>%
    filter(abs_value > correl_threshold) %>%
    group_by(var_1) %>% 
    mutate(sum_1 = sum(abs_value)) %>% 
    ungroup() %>% 
    group_by(variable) %>% 
    mutate(sum_2 = sum(abs_value)) %>% 
    ungroup() %>% 
    mutate(reject = ifelse(sum_1 > sum_2, var_1, as.character(variable))) %>% 
    distinct(reject)
  
  return(reject_variables_vector$reject)
  
}

cor_mtx <- cor(df)

df <- select(df, -c(remove_high_cor(cor_mtx)))

ggcorrplot(cor(df),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method="square",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlation Matrix")

best_k <- find_best_k(df, 19, nstart = 20)

# ploting withinss and betweenss graph and its best k ----
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
  geom_label(aes(x = which.min(best_k$best_fit) + 2, 
                 y = best_k$withinss[which.min(best_k$best_fit)] + 4, 
                 label = paste('best k is ', which.min(best_k$best_fit))
  ))

# number os clusters best fit ----
## calculating kmeans based on the best k found above
KMeans_clustering <- kmeans(df, which.min(best_k$best_fit), nstart = 20)
KMeans_clustering <- kmeans(df, 5, nstart = 20)

## Visualizing clusters
plotcluster(df, KMeans_clustering$cluster)
fviz_cluster(list(data = df, cluster = KMeans_clustering$cluster),
             show.clust.cent = T)

cluster_id_2 <- KMeans_clustering$cluster

# adding cluster information to the original data ----
df <- read_xlsx('data/6-metodos-matriciais-e-analise-de-cluster/base_sanduiches.xlsx')

# preparing dataset for cluster algorithim ----
sapply(df, class) %>% as.data.frame
df <- select(df, -c(Sanduíches))
df <- mutate_all(df, as.numeric)
df <- lapply(df, scale) %>% as_tibble


res.pca <- prcomp(df, scale = TRUE)

res.pca$x

df <- res.pca$x

best_k <- find_best_k(df, 19, nstart = 20)

# number os clusters best fit ----
## calculating kmeans based on the best k found above
KMeans_clustering <- kmeans(df, which.min(best_k$best_fit), nstart = 20)
KMeans_clustering <- kmeans(df, 5, nstart = 20)

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
  geom_label(aes(x = which.min(best_k$best_fit) + 2, 
                 y = best_k$withinss[which.min(best_k$best_fit)] + 4, 
                 label = paste('best k is ', which.min(best_k$best_fit))
  ))

## Visualizing clusters
plotcluster(df, KMeans_clustering$cluster)
fviz_cluster(list(data = df, cluster = KMeans_clustering$cluster),
             show.clust.cent = T)

cluster_id_3 <- KMeans_clustering$cluster



## Playing with PCA Analisys ----
df <- read_xlsx('data/6-metodos-matriciais-e-analise-de-cluster/base_sanduiches.xlsx')

df$cluster_id_1 <- cluster_id_1
df$cluster_id_2 <- cluster_id_2
df$cluster_id_3 <- cluster_id_3

fviz_eig(res.pca)

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF",
                col.ind = "#696969")
