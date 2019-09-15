# loading required libraries --------------------------------------------------

# libraries for matrix methods
#library(ggmcmc)
library(markovchain)

# libraries for plot
library(plotly)


# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

clearEnv()

# Atividade individual 1 ------------------------------------------------------
# criando a matriz de transição do e-commerce
matriz_transicao <- matrix(c(0.00, 0.15, 0.20, 0.00, 0.00,
                             0.10, 0.00, 0.10, 0.00, 0.00,
                             0.10, 0.05, 0.00, 0.00, 0.00,
                             0.20, 0.35, 0.13, 1.00, 0.00,
                             0.60, 0.45, 0.57, 0.00, 1.00), 
                           5, 5)

# este passo não é necessário quando
rownames(matriz_transicao) <- c("Site", "Hotpage", "CallCenter", "Venda", "Saida")
colnames(matriz_transicao) <- c("Site", "Hotpage", "CallCenter", "Venda", "Saida")

matriz_transicao

# criando a DTMC (Discrete Time Markov Chain)
dtmc_ecommerce <- new("markovchain", 
                      transitionMatrix = matriz_transicao,
                      states = c("Site", "Hotpage", "CallCenter", "Venda", "Saida"),
                      name = "Markov Chain para o e-commerce") 
dtmc_ecommerce
plot(dtmc_ecommerce, edge.arrow.size = 0.5)

# calculando a probabilidade
transitionProbability(dtmc_ecommerce, "Site", "Venda")
transitionProbability(dtmc_ecommerce, "Hotpage", "Venda")
transitionProbability(dtmc_ecommerce, "CallCenter", "Venda")

# obtendo a estabilidade final da matriz
steadyStates(dtmc_ecommerce)

# createSequenceMatrix(dtmc_ecommerce)