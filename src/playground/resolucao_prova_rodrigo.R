
# questao 1 ----

library(tibble)
library(dplyr)

tm <- matrix(c(0.00, 0.25, 0.20, 0.15, 0.40,
               0.10, 0.10, 0.20, 0.30, 0.30,
               0.20, 0.05, 0.05, 0.10, 0.60,
               0.00, 0.00, 0.00, 1.00, 0.00,
               0.00, 0.00, 0.00, 0.00, 1.00),
             nrow = 5,
             byrow = TRUE)
tm

library(markovchain)

mkv_chain <- new("markovchain", 
                 transitionMatrix = tm,
                 states = c("Site", "Hotpage", "Call Center", "Venda", 'Saida'),
                 name = "MarkovChain para o e-commerce") 
mkv_chain

plot(mkv_chain, edge.arrow.size = 0.5)

#  Responda as atividades abaixo:

## item a ----

get_transition_states <- function(initial_state, steps, mkv_chain) {
  
  states <- initial_state
  
  for (step in seq(1, steps, 1)) {
    final_state <- initial_state * (mkv_chain ^ step)
    states <- rbind(states, final_state)
  }

  states <- as_tibble(states)
  states$step <- as.numeric(row.names(states)) - 1
  states <- select(states, step, everything())
  
  output = list()
  
  output$transition_states <- states

  return(output)
}

### subitem i ----

initial_state <- c(1, 0, 0, 0, 0) # vetor semente para cliente iniciando no site
steps <- 1000

tail(get_transition_states(initial_state, steps, mkv_chain)$transition_states, 1)

### subitem ii ----

initial_state <- c(0, 1, 0, 0, 0) # vetor semente para cliente iniciando na hotpage
steps <- 1000

tail(get_transition_states(initial_state, steps, mkv_chain)$transition_states, 1)

### subitem iii ----

initial_state <- c(0, 0, 1, 0, 0) # vetor semente para cliente iniciando no call center
steps <- 1000

tail(get_transition_states(initial_state, steps, mkv_chain)$transition_states, 1)


## item b ----

### O melhor canal é a hotpage pois apresenta uma taxa de conversão de aproximadamente 40%

## item b ----

initial_state <- c(0.5, 0.2, 0.3, 0, 0) # vetor semente para 50% nosite, 20% na hotpage e 30% no call center
steps <- 5                              # 5 steps conforme enunciado da prova

get_transition_states(initial_state, steps, mkv_chain)$transition_states

steps <- 1000                           # estabilidade

tail(get_transition_states(initial_state, steps, mkv_chain)$transition_states, 1)

##################################################################################
#--------------------------------------------------------------------------------#
##################################################################################

# questao 4 ----

## item a ----

library(readxl)
library(ggcorrplot)

df <- read_xlsx('data/raw/base_sanduiches.xlsx')

# preparando o dataset
sapply(df, class) %>% as.data.frame
df <- select(df, -c(Sanduíches))
df <- mutate_all(df, as.numeric)
df_z <- lapply(df, scale) %>% as_tibble

# funcao customizada para encontrar o melhor quantidade de clustesr
find_best_k <- function(x, k_limit, nstart) {

  metrics <- tibble()

  for (k in seq(1, k_limit, 1)) {
    temp <- kmeans(x, k, nstart = nstart)
    df_temp <- tibble(k = k,
                      withinss = temp$tot.withinss,
                      betweenss = temp$betweenss)
    metrics <- bind_rows(metrics, df_temp)
  }

  return(metrics)

}

best_k <- find_best_k(df_z, 19, nstart = 20)

# plotando withinss e betweenss
ggplot(data = best_k, aes(x = k)) +
  geom_point(aes(y = withinss), color = 'blue') +
  geom_point(aes(y = betweenss), color = 'red') +
  geom_line(aes(y = withinss), color = 'blue') +
  geom_line(aes(y = betweenss), color = 'red')

# analizando visualmente selecionamos 5 como a quantidade de clusters ideal
# pois esta exatamento no meio do cotovelo apresentado pelas metricas withinss e betweenss

clusters <- kmeans(df_z, 5, nstart = 20)

## Visualizing clusters

library(fpc)
plotcluster(df_z, clusters$cluster)

library(factoextra)
fviz_cluster(list(data = df_z, cluster = clusters$cluster),
             show.clust.cent = T)

## item b ----

df_z_d <- as.matrix(dist(df, method = "euclidean"))

library(cluster)
md <- daisy(df_z_d)
md

cluster.stats(md, clusters$cluster)

sil = silhouette(clusters$cluster, md)

plot(sil, cex=.6, border = 1, col = "lightblue")

# o ASW foi de 0,18 indicando uma estrutura fraca, provavelmente artificial.
