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

# getting only vars meaningful as clustering drivers
# filial não é porque é apenas o nome da filial
# aval_global não é porque é uma composição das outras variáveis
# idade também não é porque não é um fator caracterizador relevante
ATIBAIA_drivers = ATIBAIA[, -c(1,2,9)]

# transforming and scaling relevant vars to get the final dataset
ATIBAIA_drivers_num <- ATIBAIA_drivers
ATIBAIA_drivers_num$biling = as.numeric(ATIBAIA_drivers_num$biling)
ATIBAIA_drivers_num$estac = as.numeric(ATIBAIA_drivers_num$estac)
ATIBAIA_drivers_num$ti = as.numeric(ATIBAIA_drivers_num$ti)

ATIBAIA_drivers_num_z <- as.data.frame(lapply(ATIBAIA_drivers_num, scale))

# calculating vars correlation
cor(ATIBAIA_drivers_num_z)

# withinss, betweenss and asw according to its K ----------------------------------

max_number_of_clusters <- nrow(ATIBAIA_drivers_num_z) - 1

# initializing the vectors
withinss_evolution  <- vector(mode = "numeric", length = max_number_of_clusters)
betweenss_evolution <- vector(mode = "numeric", length = max_number_of_clusters)

for (count in seq(1, max_number_of_clusters, by = 1)) {
  
  number_of_clusters <- as.numeric(count)
  KMeans_clustering <- kmeans(ATIBAIA_drivers_num_z, number_of_clusters, nstart = 20)
  withinss_evolution[count] <- sum(KMeans_clustering$withinss)
  betweenss_evolution[count] <- KMeans_clustering$betweenss
}

plot(withinss_evolution, type = "b")
lines(t, w, type="b", col="red", lwd=2, pch=19)

plot(betweenss_evolution, type = "b")
lines(t, w, type="b", col="red", lwd=2, pch=19)