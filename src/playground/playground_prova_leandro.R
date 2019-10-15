#libraries for file and data manipulation
library(readxl)
library(dplyr)

# cluster analysis
library(fpc)
library(factoextra)
library(cluster)
library(ggcorrplot)
library(reshape2)

# Carregando e realizando preparação dos dados --------------------------------
SANDUICHES <- read_xlsx("data/raw/base_sanduiches.xlsx", sheet = "SANDUICHES", col_names = TRUE)

# Mapear os tipos dos dados das colunas da tabela importada.
sapply(SANDUICHES, class)

# Sumarizar as características univariadas dos dados da tabela importada.
summary(SANDUICHES)

# Selecionar somente as variáveis drivers de clustering 
SANDUICHES_drivers <- SANDUICHES[ , -c(1)]
SANDUICHES_drivers <- mutate_all(SANDUICHES_drivers, as.numeric)

# Transformando tudo para tipo numérico e colocando as colunas em escala.
SANDUICHES_drivers_num_z <- as.data.frame(lapply(SANDUICHES_drivers, scale))

# Análise de Clusters POR PARTIÇÃO: K-Means -----------------------------------

# Visualizando a correlação
ggcorrplot(cor(SANDUICHES_drivers_num_z),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method="square",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlation Matrix for Sanduiches")

# A função mais utilizada para operacionalizar o K-Means é a kmeans, 
# nativa do R vinculada à biblioteca stats, também nativa do R. Por ser 
# uma biblioteca nativa, não há necessidade de carregar a biblioteca pois 
# a mesma já é naturalmete carregada.
KMeans_clustering_k3 <- kmeans(SANDUICHES_drivers_num_z, 3, nstart = 20)
KMeans_clustering_k3

# Vetor de clusters finais.
KMeans_clustering_k3$cluster

# Centróides dos clusters.
KMeans_clustering_k3$centers

# Soma total de distâncias quadráticas (Total Sum of Squares).
KMeans_clustering_k3$totss

# Soma de distâncias quadráticas internas a cada cluster (Whithin Sum of Squares).
KMeans_clustering_k3$withinss

# Soma total de distâncias quadráticas internas a cada cluster (Total Whithin Sum of Squares).
KMeans_clustering_k3$tot.withinss

# Soma total de distâncias quadráticas entre observações de clusters diferentes (Between Sum of Squares).
KMeans_clustering_k3$betweenss

# Tamanho dos clusters.
KMeans_clustering_k3$size

# Iteraçoes para que se chegasse ao resultado final.
KMeans_clustering_k3$iter

# Indicação de se houve alguma falha na execução do algoritmo.
KMeans_clustering_k3$ifault

# Utilizar a função plotcluster do pacote fpc, que imprime o 
# gráfico espacial nas duas componentes principais mais relevantes.
plotcluster(SANDUICHES_drivers_num_z, KMeans_clustering_k3$cluster)

fviz_cluster(list(data = SANDUICHES_drivers_num_z, cluster = KMeans_clustering_k3$cluster),  show.clust.cent = T)

# Calcular a matriz de distâncias.
SANDUICHES_drivers_num_z_dist <- dist(SANDUICHES_drivers_num_z, method = "euclidean")
SANDUICHES_drivers_num_z_dist

# Utilizar a função cluster.stats da library fpc para uma lista maior de métricas da solução de clusters encontrada.
cluster.stats(SANDUICHES_drivers_num_z_dist, KMeans_clustering_k3$cluster)

# Algoritmos do Pacote 'clusters' ---------------------------------------------

# Padronizar as variáveis e calcular a matriz de distancias por métrica de Gower.
md = daisy(SANDUICHES_drivers)

# Usa-se a função pam (Partitioning Around Medoids) do pacote cluster
KMedoid_clustering <- pam(md, k = 3, diss = TRUE)
KMedoid_clustering

# A entrada para a função pam pode ser tanto a matriz de distâncias (caso anterior), 
# quanto a matriz de observações (x) por variáveis (y) (como no caso abaixo). 
# Deve-se alterar o parâmetro diss para FALSE).
KMedoid_clustering <- pam(SANDUICHES_drivers_num_z, k = 3, diss = FALSE)
KMedoid_clustering

# Imprimir os medoids finais.
KMedoid_clustering$medoids

# Imprimir os ids dos medoids finais.
KMedoid_clustering$id.med

# Vetor de clusters de cada observação.
KMedoid_clustering$clustering

# Função objetivo do método de construção e swap (algoritmos de otimização que são executados pré K-Medoid para que a escolha inicial de medoids seja melhorada.
KMedoid_clustering$objective

# Indicação se algum dos clusters pode ser classificado como isolado (quando o diâmetro é maior que a separação de clusters ou outras relações desfavoráveis de métricas).
KMedoid_clustering$isolation

# Informações de distância.
KMedoid_clustering$clusinfo

# Informação de silhouette.
KMedoid_clustering$silinfo

# Matriz de distâncias de distâncias.
KMedoid_clustering$diss

# Chamada de função originadora.
KMedoid_clustering$call

# Jeito mais fácil de imprimir o gráfico de silhouette.
plot(KMedoid_clustering)

# Outra maneira de obter dados de silhouette e imprimir o gráfico correspondente.
sil <- silhouette(KMedoid_clustering, md)
head(sil[,1:3])
plot(sil, cex=.6, border = 1, col = "lightblue")

# Utilizar a função cluster.stats da library fpc para uma lista maior de métricas da solução de clusters encontrada.
cluster.stats(SANDUICHES_drivers_num_z_dist, KMedoid_clustering$cluster)

# Análise de Clusters HIERÁRQUICOS: AGNES -------------------------------------

agnes_clustering <- agnes(md, diss = TRUE, method = "complete")
agnes_clustering

# Imprime a ordem das observações, guardadas para facilitar a impressão do dendograma.
agnes_clustering$order

# Imprime a altura de aglomeração para as observações conforme vão acontecendo.
agnes_clustering$height

agnes_clustering$ac

agnes_clustering$merge

agnes_clustering$diss

agnes_clustering$call

agnes_clustering$method

plot(agnes_clustering)

agnes_clustering_k3 <- cutree(agnes_clustering, k = 3)
agnes_clustering_k3

fviz_dend(agnes_clustering, k=3)

fviz_cluster(list(data = SANDUICHES_drivers_num_z, cluster = agnes_clustering_k3),  show.clust.cent = F)

# Utilizar a função cluster.stats da library fpc para uma lista maior
# de métricas da solução de clusters encontrada.
cluster.stats(SANDUICHES_drivers_num_z_dist, agnes_clustering_k3)

# Análise de Clusters HIERÁRQUICOS: DIANA -------------------------------------

diana_clustering <- diana(md, diss = TRUE)
diana_clustering

# Imprime a ordem das observações, guardadas para facilitar a impressão do dendograma.
diana_clustering$order

# Imprime a altura de aglomeração para as observações conforme vão acontecendo.
diana_clustering$height

# Imprime o coeficiente de 
diana_clustering$dc

diana_clustering$merge

diana_clustering$diss

plot(diana_clustering)

diana_clustering_k3 <- cutree(diana_clustering, k = 3)
diana_clustering_k3

fviz_dend(agnes_clustering, k=3)

fviz_cluster(list(data = SANDUICHES_drivers_num_z, cluster = agnes_clustering_k3),  show.clust.cent = F)

# Utilizar a função cluster.stats da library fpc para uma lista maior 
# de métricas da solução de clusters encontrada.
cluster.stats(SANDUICHES_drivers_num_z_dist, diana_clustering_k3)

# Definindo withinss e betweenss de acordo com seu K --------------------------

GetBestK <- function(data, max_k, nstart_value)
{
  results <- tibble(k = numeric(), 
                    withinss = numeric(), 
                    betweenss = numeric(), 
                    best_fit = numeric())
  
  for (count in seq(1, max_k, by = 1)) {
    number_of_clusters <- as.numeric(count)
    KMeans_clustering <- kmeans(data, number_of_clusters, nstart = nstart_value)
    
    results <- add_row(results,
                       k = number_of_clusters,
                       withinss = sum(KMeans_clustering$withinss),
                       betweenss = KMeans_clustering$betweenss,
                       best_fit = abs(sum(KMeans_clustering$withinss) - KMeans_clustering$betweenss))
  }
  
  return(results)
}

max_number_of_clusters <- nrow(SANDUICHES_drivers_num_z) - 1

best_k <- GetBestK(SANDUICHES_drivers_num_z, max_number_of_clusters, 20)

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
                 label = paste('best k is ', which.min(best_k$best_fit))))

# choosing the K that best fits
KMeans_clustering <- kmeans(SANDUICHES_drivers_num_z, which.min(best_k$best_fit), nstart = 20)

# Visualizing clusters
plotcluster(SANDUICHES_drivers_num_z, KMeans_clustering$cluster)
fviz_cluster(list(data = SANDUICHES_drivers_num_z, 
                  cluster = KMeans_clustering$cluster),
             show.clust.cent = T)

# olhando para outros valores de K
KMeans_clustering <- kmeans(SANDUICHES_drivers_num_z, 6, nstart = 20)

# Visualizing clusters
plotcluster(SANDUICHES_drivers_num_z, KMeans_clustering$cluster)
fviz_cluster(list(data = SANDUICHES_drivers_num_z, 
                  cluster = KMeans_clustering$cluster),
             show.clust.cent = T)

# Tentando remover auta correlação entre as variáveis -------------------------

# Carregando e realizando preparação dos dados
SANDUICHES <- read_xlsx("base_sanduiches.xlsx", sheet = "SANDUICHES", col_names = TRUE)

# Mapear os tipos dos dados das colunas da tabela importada.
sapply(SANDUICHES, class)

# Sumarizar as características univariadas dos dados da tabela importada.
summary(SANDUICHES)

# Selecionar somente as variáveis drivers de clustering 
SANDUICHES_drivers <- SANDUICHES[ , -c(1)]
SANDUICHES_drivers <- mutate_all(SANDUICHES_drivers, as.numeric)

# Transformando tudo para tipo numérico e colocando as colunas em escala.
SANDUICHES_drivers_num_z <- as.data.frame(lapply(SANDUICHES_drivers, scale))

# Função para remoção de variáveis auto correlacionadas dado um threshold
RemoveHighCorrelation <- function(cor_mtx, correl_threshold = threshold) {
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

# Calculando a correlação entre as variáveis. 
# Deve-se tomar o cuidado de que não hajam grandes correlações entre
# as variáveis, pois estaríamos considerando a mesma informação repetidas 
# vezes, aumentando o "peso" daquela informação na formação dos clusters.
var_cor <- cor(SANDUICHES_drivers_num_z)

threshold <- 0.75
SANDUICHES_drivers_num_z <- select(SANDUICHES_drivers_num_z, -c(RemoveHighCorrelation(var_cor, threshold)))

# Visualizando a correlação após remover variáreis com auto correlação
ggcorrplot(cor(SANDUICHES_drivers_num_z),
           hc.order = TRUE,
           lab = TRUE,
           lab_size = 3,
           method = "square",
           colors = c("tomato2", "white", "springgreen3"),
           title = "Correlation Matrix After Applying Threshold")

# Reavaliando withinss e betweenss de acordo com seu K
max_number_of_clusters <- nrow(SANDUICHES_drivers_num_z) - 1

best_k <- GetBestK(SANDUICHES_drivers_num_z, max_number_of_clusters, 20)

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
                 label = paste('best k is ', which.min(best_k$best_fit))))

# choosing the K that best fits
KMeans_clustering <- kmeans(SANDUICHES_drivers_num_z, which.min(best_k$best_fit), nstart = 20)

# Visualizing clusters
plotcluster(SANDUICHES_drivers_num_z, KMeans_clustering$cluster)
fviz_cluster(list(data = SANDUICHES_drivers_num_z, 
                  cluster = KMeans_clustering$cluster),
             show.clust.cent = T)

# olhando para outros valores de K
KMeans_clustering <- kmeans(SANDUICHES_drivers_num_z, 6, nstart = 20)

# Visualizing clusters
plotcluster(SANDUICHES_drivers_num_z, KMeans_clustering$cluster)
fviz_cluster(list(data = SANDUICHES_drivers_num_z, 
                  cluster = KMeans_clustering$cluster),
             show.clust.cent = T)
