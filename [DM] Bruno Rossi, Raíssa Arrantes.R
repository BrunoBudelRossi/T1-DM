setwd("C:\\Users\\bruno\\OneDrive - NuageIT\\Área de Trabalho\\Mineração de Dados")

df1 <- read.csv("./_ASSOC_BGFriends_01.csv")

df2 <- read.csv("./_ASSOC_BGFriends_02.csv")

# concatena 2 data frames por linha (rows)
df <- rbind(df1, df2)
