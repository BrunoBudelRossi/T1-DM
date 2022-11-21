library(arules)

setwd("C:\\Users\\bruno\\OneDrive - NuageIT\\Área de Trabalho\\Mineração de Dados\\T1---DM")

df1 <- read.csv("./_ASSOC_BGFriends_01.csv")

df2 <- read.csv("./_ASSOC_BGFriends_02.csv")

df <- rbind(df1, df2)

names <- c()
for (i in 1:nrow(df)) {
  row <- df[i,]
  row$Jogadore.a.s <- str_replace_all(row$Jogadore.a.s,"\xe7", "ç")
  name <- iconv(tolower(gsub(" ","", row$Jogadore.a.s)), to = "ASCII//TRANSLIT")
  
  splited <- unlist(strsplit(name, split = ","))
  names <- c(names, splited)
}
names <- unique(names)
names

columns= c(unlist(names), "Win")
results = data.frame(matrix(nrow = 0, ncol = length(names)+1 )) 
colnames(results) = columns

results

for (i in 1:nrow(df)) {
  if (as.numeric(row$Amigos) == as.numeric(row$Oponentes)) {
    next
  }
  
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

results


rules <- apriori(results, 
                 parameter = list(supp = 0.1, conf = 0.9, target = "rules"))
summary(rules)












# flag <- TRUE
# for (i in 1:nrow(df)) {
#   row <- df[i,]
#   
#   # testar se é igual
#   isWin <- row$Amigos > row$Oponentes
#   
#   name <- gsub(" ","", row$Jogadore.a.s)
#   
#   if (nrow(results) != 0) {
#     for (j in 1:nrow(results)) {
#       row2 <- results[j,]
#       if (name == row2$names) {
#         if (isWin) {
#           results[j, 2] <- as.numeric(results[j, 2]) + 1
#         } else {
#           results[j, 3] <- as.numeric(results[j, 3]) + 1
#         }
#         flag <- FALSE
#       }
#     }
#   }
#   
#   if (flag) {
#     if (isWin) {
#       results[nrow(results) + 1,] = c(name, 1, 0)
#     } else {
#       results[nrow(results) + 1,] = c(name, 0, 1)
#     }
#   }
#   flag <- TRUE
# }
# results$wins <- as.numeric(as.character(results$wins))
# results$defeats <- as.numeric(as.character(results$defeats))
# orderResults <- results[order(results$wins, decreasing = TRUE),]



