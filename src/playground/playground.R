# loading required libraries
library(tibble)
library(dplyr)
library(readr)
library(reshape2)
library(markovchain)
library(Matrix)
library(matrixcalc)
library(network)
library(visNetwork)
library(networkD3)


# load data ----
df <- readRDS('data/raw/skillset.rds')
tm <- select(df, name, skill)
# df <- df[1:50, ] # uncomment to reduce tha data amount for testing
print(paste('Number of Unique Skills:    ', n_distinct(tm$skill)))
print(paste('Number of Unique Employees: ', n_distinct(tm$name)))

# get the cartesian product by name and create a transition matrix containing the number of unique employees with its skill combination. ----
tm <- left_join(tm, tm, by = c('name'))
tm <- acast(tm, skill.x ~ skill.y, fun.aggregate = n_distinct, value.var = 'name')
tm[1:5, 1:5]

# get the list of skill combinations ----
skill_combinations <- t(combn(unique(df$skill), 2))
skill_combinations[1:10, ]

# check if transition matrix is symetric (expected for this usecase) and square ----
print(paste('Is the Transition Matrix Symetric? ', isSymmetric(tm)))
print(paste('Is the Transition Matrix Square?   ', is.square.matrix(tm)))

# cating 1 to the transition matrix ----
diag(tm) <- 1
tm[1:5, 1:5]
print(paste('Is the Transition Matrix Symetric? ', isSymmetric(tm)))
print(paste('Is the Transition Matrix Square?   ', is.square.matrix(tm)))

# calculating the row proportion ----
tm = tm / colSums(tm)
tm[1:5, 1:5]

# creating the markov chain object ----
mkv <- new('markovchain', transitionMatrix = tm, 
           states = rownames(tm))

# calculating the markov chain steady states ----
steady <- t(steadyStates(mkv))
steady <- as_tibble(steady, rownames = 'skills')

# calculating the eigenvectors and eigenvalues for the transition matrix. ----
eigen_v <- eigen(tm)

# ploting top 25 skill relationships ----
top_x = top_n(steady, 25, V11)$skills
edges <- as_tibble(tm, rownames = 'from')
edges <- melt(edges, id.vars = 'from')
edges <- group_by(edges, from)
edges <- arrange(edges, desc(value))
edges <- filter(edges, from %in% top_x)
edges <- filter(edges, row_number() <= 5, value > 0, from != variable)
colnames(edges) <- c('from', 'to', 'weight')
edges$from <- as.character(edges$from)
edges$to <- as.character(edges$to)
edges$weight <- as.double(edges$weight)

nodes2 <- distinct(edges, from)
colnames(nodes2) <- c('label')
nodes3 <- distinct(edges, to)[,1]
colnames(nodes3) <- c('label')
nodes2 <- merge(nodes2, nodes3 , all.x=TRUE, all.y=TRUE)
nodes2 <- distinct(nodes2)
nodes2$id <- row_number(nodes2)
nodes <- select(nodes2, id, everything())
rm(nodes2, nodes3)

edges$from <- plyr::mapvalues(edges$from, nodes$label, nodes$id, warn_missing = FALSE)
edges$to <- plyr::mapvalues(edges$to, nodes$label, nodes$id, warn_missing = FALSE)

edges$width <- edges$weight * 100

routes_network_1 <- network(edges, matrix.type = "edgelist", ignore.eval = FALSE)

plot(routes_network_1)

visNetwork(nodes, edges)

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_dh")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_nicely")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_in_circle")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_on_sphere")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_on_grid")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_gem")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_with_graphopt")

visNetwork(nodes, edges) %>% 
  visIgraphLayout(layout = "layout_in_circle")



