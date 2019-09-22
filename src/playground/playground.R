# loading required libraries
library(tibble)
library(dplyr)
library(readr)
library(stringr)
library(reshape2)
library(markovchain)
library(Matrix)
library(matrixcalc)
library(visNetwork)

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

# cating 1 to the transition matrix ----
diag(tm) <- 1
tm[1:5, 1:5]

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

# Custom plotting function ----
# use this functions to plot a list of skills with its related n skills.
# data frame passed to the function can e filtered to specific scopes (city, client, etc)
plot_net_vis <- function(df, skills, n_relation) {
  edges <- select(df, name, skill)
  edges <- left_join(edges, edges, by = c('name'))
  edges <- acast(edges, skill.x ~ skill.y, fun.aggregate = n_distinct, value.var = 'name')
  diag(edges) <- 0
  edges <- as_tibble(edges, rownames = 'from')
  edges <- melt(edges, id.vars = 'from')
  edges <- group_by(edges, from)
  edges <- arrange(edges, desc(value))
  edges <- filter(edges, from %in% skills)
  edges <- filter(edges, row_number() <= n_relation, value > 0, from != variable)
  colnames(edges) <- c('from', 'to', 'weight')
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  edges$weight <- as.double(edges$weight)
  nodes2 <- distinct(edges, from)
  colnames(nodes2) <- c('label')
  nodes3 <- distinct(edges, to)[ ,1]
  colnames(nodes3) <- c('label')
  nodes2 <- merge(nodes2, nodes3 , all.x=TRUE, all.y=TRUE)
  nodes2 <- distinct(nodes2)
  nodes2$id <- row_number(nodes2)
  nodes <- select(nodes2, id, everything())
  rm(nodes2, nodes3)
  edges$from <- plyr::mapvalues(edges$from, nodes$label, nodes$id, warn_missing = FALSE)
  edges$to <- plyr::mapvalues(edges$to, nodes$label, nodes$id, warn_missing = FALSE)
  edge_count <- group_by(edges, to) %>%
    summarise(value = log(n() + 1),
              title = paste('Receives', n(), 'connections', sep = ' ')) %>%
    arrange(desc(value)) %>% as_tibble(.name_repair = TRUE)
  edge_count$to <- as.integer(edge_count$to)
  nodes <- left_join(nodes, edge_count, by = c('id' = 'to'))
  nodes$value <- ifelse(is.na(nodes$value), 1, nodes$value)
  visNetwork(nodes, edges) %>% 
    visNodes(physics = TRUE, fixed = FALSE, 
             shape = 'dot', borderWidthSelected = 3) %>% 
    visEdges(arrows = 'to', smooth = 'forceDirection', 
             width = 1, shadow = TRUE)
}


# plotting big data and data science related skills ----

df_filter <- df

# getting the list of related skills
skill_filter <- distinct(df, skill) %>% arrange(skill)
skill_filter <- filter(skill_filter, 
                 str_detect(skill, 'DATA SCIENCE|BIG DATA|MACHINE LEARNING|DATA ANALYTICS|DEEP LEARNING|COGNITIVE|NATURALLANGUAGE'))

# calling our custom function
plot_net_vis(df_filter, skill_filter$skill, 10)
