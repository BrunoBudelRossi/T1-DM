setwd("C:\\Users\\bruno\\OneDrive - NuageIT\\Área de Trabalho\\Mineração de Dados\\T1---DM")

getwd()

df1 <- read.csv("./_ASSOC_BGFriends_01.csv")

df2 <- read.csv("./_ASSOC_BGFriends_02.csv")

df <- rbind(df1, df2)

columns= c("names","wins", "defeats") 
results = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(results) = columns

flag <- TRUE
for (i in 1:nrow(df)) {
  row <- df[i,]
  
  isWin <- row$Amigos > row$Oponentes
  
  name <- gsub(" ","", row$Jogadore.a.s)
  
  if (nrow(results) != 0) {
    for (j in 1:nrow(results)) {
      row2 <- results[j,]
      if (name == row2$names) {
        if (isWin) {
          results[j, 2] <- as.numeric(results[j, 2]) + 1
        } else {
          results[j, 3] <- as.numeric(results[j, 3]) + 1
        }
        flag <- FALSE
      }
    }
  }
  
  if (flag) {
    if (isWin) {
      results[nrow(results) + 1,] = c(name, 1, 0)
    } else {
      results[nrow(results) + 1,] = c(name, 0, 1)
    }
  }
  flag <- TRUE
}
results$wins <- as.numeric(as.character(results$wins))
results$defeats <- as.numeric(as.character(results$defeats))
orderResults <- results[order(results$wins, decreasing = TRUE),]



