# performing data loading -----------------------------------------------------
dataDirectory <- "../data/raw/"

target_data <- read_xlsx(paste(dataDirectory, "base_casamentos.xlsx", sep = ""), sheet = "CASAMENTOS", col_names = TRUE)