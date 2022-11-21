library(arules)
library(stringr)

setwd("C:\\Users\\bruno\\OneDrive - NuageIT\\Área de Trabalho\\Mineração de Dados\\T1---DM")

df1 <- read.csv("./_ASSOC_BGFriends_01.csv", stringsAsFactors = TRUE)

df2 <- read.csv("./_ASSOC_BGFriends_02.csv", stringsAsFactors = TRUE)

df <- rbind(df1, df2)

names <- c()
for (i in 1:nrow(df)) {
  row <- df[i,]
  row$Jogadore.a.s <- str_replace_all(row$Jogadore.a.s,"\xe7", "ç")
  name <- iconv(tolower(gsub(" ","", row$Jogadore.a.s)), to = "ASCII//TRANSLIT")
  
  splited <- unlist(strsplit(name, split = ","))
  names <- c(names, splited)
}
names <- unique(unlist(names))

columns <- c(names, "Win")
results <- data.frame(matrix(nrow = 0, ncol = length(names)+1 ), stringsAsFactors = TRUE) 
colnames(results) <- columns

for (i in 1:nrow(df)) {
  row <- df[i,]
  row$Jogadore.a.s <- str_replace_all(row$Jogadore.a.s,"\xe7", "ç")
  name <- iconv(tolower(gsub(" ","", row$Jogadore.a.s)), to = "ASCII//TRANSLIT")
  
  splited <- unlist(strsplit(name, split = ","))
  
  isWin <- row$Amigos > row$Oponentes 
  
  for (j in 1:length(splited)) {
    results[i, splited[j]] <- 1
    results[i, "Win"] <- isWin
  }
}
results[is.na(results)] <- 0

for (i in 1:dim(results)[2]) {
  results[,i] <- as.factor(results[,i])
}


rules <- apriori(results, parameter = list(supp = 0.1, conf = 0.5, target = "rules", minlen = 2, maxlen = 2))

resSubset <- subset(rules, rhs %in% "Win=TRUE")

sorted <- sort(resSubset, by="confidence") 

inspect(sorted)







