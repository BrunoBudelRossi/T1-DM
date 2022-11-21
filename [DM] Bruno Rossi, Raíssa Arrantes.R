library(arules)
library(stringr)

handleNames <- function(name) {
  name <- str_replace_all(name,"\xe7", "ç")
  name <- iconv(tolower(gsub(" ","", name)), to = "ASCII//TRANSLIT")
  return(unlist(strsplit(name, split = ","))) 
}

setwd("C:\\Users\\bruno\\OneDrive - NuageIT\\Área de Trabalho\\Mineração de Dados\\T1---DM")

df1 <- read.csv("./_ASSOC_BGFriends_01.csv", stringsAsFactors = TRUE)

df2 <- read.csv("./_ASSOC_BGFriends_02.csv", stringsAsFactors = TRUE)

df <- rbind(df1, df2)

names <- c()
for (i in 1:nrow(df)) {
  row <- df[i,]
  splited <- handleNames(row$Jogadore.a.s)
  names <- c(names, splited)
}
names <- unique(unlist(names))

columns <- c(names, "Win")
results <- data.frame(matrix(nrow = 0, ncol = length(names)+1 ), stringsAsFactors = TRUE) 
colnames(results) <- columns

draws <- c()
for (i in 1:nrow(df)) {
  row <- df[i,]
  if (row$Amigos == row$Oponentes) {
    draws <- c(draws, row$Partida)
  }
  
  splited <- splited <- handleNames(row$Jogadore.a.s)
  isWin <- row$Amigos > row$Oponentes 
  
  for (j in 1:length(splited)) {
    results[i, splited[j]] <- 1
    results[i, "Win"] <- isWin
  }
}

for (i in draws) {
  results <- results[-i,]
}

results[is.na(results)] <- 0

results

for (i in 1:dim(results)[2]) {
  results[,i] <- as.factor(results[,i])
}

rules <- apriori(results, parameter = list(supp = 0.1, conf = 0.5, target = "rules", minlen = 3, maxlen = 3))

bestSquad <- subset(rules, rhs %in% "Win=TRUE")

inspect(sort(bestSquad, by="confidence")[1:5])

worstSquad <- subset(rules, rhs %in% "Win=FALSE")

inspect(sort(worstSquad, by="confidence")[1:5])

rules <- apriori(results, parameter = list(supp = 0.1, conf = 0.5, target = "rules", minlen = 2, maxlen = 2))

bestPlayer <- subset(rules, rhs %in% "Win=TRUE")

inspect(sort(bestPlayer, by="confidence")[1:5])





