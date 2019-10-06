# loading required libraries --------------------------------------------------

#libraries for file manipulation
library(readxl)

# cluster analysis
library(fpc)
library(factoextra)
library(cluster)

# loading other scripts do be used here ---------------------------------------
source("./src/util/auxiliary_functions.R")

clearEnv()

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

# getting a summary to check the preparation
summary(ATIBAIA)

# getting only vars meaningful as clustering drivers
# filial não é porque é apenas o nome da filial
# aval_global não é porque é uma composição das outras variáveis
# idade também não é porque não é um fator caracterizador relevante
ATIBAIA_drivers = ATIBAIA[ , -c(1,2,9)]

# transforming and scaling relevant vars to get the final dataset
ATIBAIA_drivers_num <- ATIBAIA_drivers
ATIBAIA_drivers_num$biling = as.numeric(ATIBAIA_drivers_num$biling)
ATIBAIA_drivers_num$estac = as.numeric(ATIBAIA_drivers_num$estac)
ATIBAIA_drivers_num$ti = as.numeric(ATIBAIA_drivers_num$ti)

ATIBAIA_drivers_num_z <- as.data.frame(lapply(ATIBAIA_drivers_num, scale))