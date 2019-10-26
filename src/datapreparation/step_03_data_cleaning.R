## ---- step_03_data_cleaning.R

# analysing missing values and other strange conditions -----------------------

# renaming columns
names(target_data)[1]  <- "id"
names(target_data)[2]  <- "aceitar_convite"
names(target_data)[3]  <- "gostar_doces"
names(target_data)[4]  <- "emocionar"
names(target_data)[5]  <- "conhecer_pessoas"
names(target_data)[6]  <- "diversao_amigos"
names(target_data)[7]  <- "gostar_fotos"
names(target_data)[8]  <- "cansar_cerimonia"
names(target_data)[9]  <- "casamento_civil"
names(target_data)[10] <- "idade"
names(target_data)[11] <- "sexo"