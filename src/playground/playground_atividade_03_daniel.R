# # Install packages (uncomment if needed) ---------------------------------------
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggcorrplot")
# install.packages("fpc")
# install.packages("factoextra")
# install.packages("data.table")
# install.packages("magrittr")

# Load packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggcorrplot)
library(fpc)
library(factoextra)
library(data.table)
library(magrittr)

# Load functions ----------------------------------------------------------------
# Function created to find the best k for cluster analysis.
# This function returns two arguments:
#   1) a data.frame containing the k, tot.withinss, betweenss and the absolute difference of withinss and betweenss
#   2) a data.frame only with best k
# Calculation note: Best k is chosen by minimum difference between withinss and betweenss

best_k <- function(df, min_k, max_k, nstart) {
  output <- data.frame()
  for (k in min_k:max_k) {
    model <- kmeans(df, k, nstart = nstart)
    output <- bind_rows(output, 
                        data.frame(k = k,
                                   totss = model$totss,
                                   tot.withinss = model$tot.withinss,
                                   betweenss = model$betweenss) %>% 
                          mutate(fit = abs(tot.withinss - betweenss)))
  }
  return(list(output=output,
              best=output %>% arrange(fit) %>% head(1)))
}

# Load data ----------------------------------------------------------------
ATIBAIA <- read_xlsx("./data/raw/ATIBAIA.xlsx", 
                     sheet = "ATIBAIA", 
                     col_names = TRUE)

# Data preparation ---------------------------------------------------------
ATIBAIA <- ATIBAIA %>% 
  select(instal, biling, estac, train, ti, social) %>% 
  mutate_all(as.numeric) %>% 
  lapply(scale) %>% 
  as.data.frame

# Correlogram plot ---------------------------------------------------------
ggcorrplot(cor(ATIBAIA),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method="square",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlation Matrix")

# Calculate best k ---------------------------------------------------------
best_k_data <- best_k(ATIBAIA, 1, 29, nstart = 20)

# Plot best k --------------------------------------------------------------
ggplot(data = best_k_data$output, aes(x = k)) +
  geom_point(aes(y = tot.withinss), color = 'blue') +
  geom_point(aes(y = betweenss), color = 'red') +
  geom_line(aes(y = tot.withinss), color = 'blue') +
  geom_line(aes(y = betweenss), color = 'red') +
  geom_segment(aes(x = which.min(best_k_data$output$fit), 
                   y = best_k_data$output$tot.withinss[which.min(best_k_data$output$fit)], 
                   xend = which.min(best_k_data$output$fit), 
                   yend = best_k_data$output$betweenss[which.min(best_k_data$output$fit)]),
               linetype = "dashed", color = "black") +
  geom_label(aes(x = which.min(best_k_data$output$fit)+2, 
                 y = best_k_data$output$tot.withinss[which.min(best_k_data$output$fit)]+6, 
                 label = paste('Best k is ', which.min(best_k_data$output$fit)))) + 
  labs(y="Withinss / Betweenss", x="Number of K clusters")

# Cluster with best k -------------------------------------------------------
cluster <- kmeans(ATIBAIA, best_k_data$best$k[1], nstart = 20)

# Clusters visualization ---------------------------------------------------- 
plotcluster(ATIBAIA, cluster$cluster)
fviz_cluster(list(data = ATIBAIA, cluster = cluster$cluster),
             show.clust.cent = T)
