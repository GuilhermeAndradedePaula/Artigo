library(BatchGetSymbols)
library(tidyr)
library(Hmisc)
library(corrplot)
library(igraph)
library(dplyr)
library(poweRlaw)
library(ggplot2)
source("http://www.sthda.com/upload/rquery_cormat.r")

getwd()
load("ibov.dta")
ibov

tickers <- paste0(ibov$tickers, ".SA")
tickers

first.date <- c("2016-01-01","2017-01-01","2018-01-01","2019-01-01","2020-01-01","2021-01-01")
last.date <- c("2016-12-31","2017-12-31","2018-12-31","2019-12-31","2020-12-31","2021-04-12")

fibov16 <- BatchGetSymbols(tickers,
                         first.date = first.date[[1]], 
                         last.date = last.date[[1]], 
                         bench.ticker = "VALE3.SA",
                         thresh.bad.data = 0.99)

fibov17 <- BatchGetSymbols(tickers,
                           first.date = first.date[[2]], 
                           last.date = last.date[[2]], 
                           bench.ticker = "VALE3.SA",
                           thresh.bad.data = 0.99)

fibov18 <- BatchGetSymbols(tickers,
                           first.date = first.date[[3]], 
                           last.date = last.date[[3]], 
                           bench.ticker = "VALE3.SA",
                           thresh.bad.data = 0.99)

fibov19 <- BatchGetSymbols(tickers,
                           first.date = first.date[[4]], 
                           last.date = last.date[[4]], 
                           bench.ticker = "VALE3.SA",
                           thresh.bad.data = 0.99)

fibov20 <- BatchGetSymbols(tickers,
                           first.date = first.date[[5]], 
                           last.date = last.date[[5]], 
                           bench.ticker = "VALE3.SA",
                           thresh.bad.data = 0.99)

fibov21 <- BatchGetSymbols(tickers,
                           first.date = first.date[[6]], 
                           last.date = last.date[[6]], 
                           bench.ticker = "VALE3.SA",
                           thresh.bad.data = 0.99)

fibov <- list(fibov16$df.tickers,fibov17$df.tickers,fibov18$df.tickers,fibov19$df.tickers,fibov20$df.tickers,fibov21$df.tickers)
fibov

i <- 0
wdblist <- list()
oplist <- list()
corlist <- list()
matcor <- list()
frlist <- list()
glist <- list()
strglist <- list()
adjlist <- list()
gadjlist <- list()
dglist <- list()
eigglist <- list()

for (x in fibov) {
  i <- i+1
  
  fibov_na <- x %>% drop_na(ret.closing.prices) #tabela com a variação de preços diários sem na
  wdb <- pivot_wider(fibov_na[,c('ref.date','ticker','ret.closing.prices')],
                     names_from = ticker, values_from = ret.closing.prices) %>% select(where(~!any(is.na(.)))) #tabela no formato wide
  wdb <- wdb[,colSums(wdb != 0) > 0] #tabela no formato wide sem as colunas que podem dar erro no calculo de correlação
  op <- colnames(wdb[-1]) #opções de cada periodo
  wdblist[[i]] <- wdb
  oplist[[i]] <- op
  
  list.cor <- rquery.cormat(select(wdb, -c('ref.date')),
                             type = 'flatten',
                            graph = FALSE) #lista de correlação usada para criação do objeto grafo
  
  mat.cor <- rquery.cormat(select(wdb, -c('ref.date')),
                           type = 'full',
                           graph = FALSE) #matriz de correlação ponderada
  matcor[[i]] <- mat.cor
  corlist[[i]] <- list.cor$r
  
  fr <- mean(list.cor$r[,3]) #calculo do fator de relevancia
  frlist[[i]] <- fr
  
  list.cor.fr <- list.cor$r[which(list.cor$r$cor >= fr),] #lista de correlação filtrada pelo fator de relevancia
  names(list.cor$r)[1] <- "from"
  names(list.cor$r)[2] <- "to"
  names(list.cor$r)[3] <- "weight"
  g0 <- graph_from_data_frame(list.cor$r[,1:3], directed = FALSE, vertices = op) #criação do objeto grafo da matriz de correlação completa
  strg <- strength(g0, vids = V(g0), mode = "all", loops = FALSE) #calculo da centralidade strength
  strglist[[i]] <- strg
  
  names(list.cor.fr)[1] <- "from"
  names(list.cor.fr)[2] <- "to"
  names(list.cor.fr)[3] <- "weight"
  g <- graph_from_data_frame(list.cor.fr[,1:3], directed = FALSE, vertices = op) #criação do objeto grafo da matriz de correlação filtrada pelo fator de relevancia de cada periodo
  glist[[i]] <- g
  
  adj.mat <- as_adjacency_matrix(g, type = "both") #criação da matriz de adjacencia
  g.adj <- graph_from_adjacency_matrix(adj.mat, mode = "undirected", weighted = NULL) #criação do objeto grafo da matriz de adjacencia
  adjlist[[i]] <- adj.mat
  gadjlist[[i]] <- g.adj
  
  dg <- degree(g.adj) #calculo da centralidade de grau
  dglist[[i]] <- dg
  
  eigg <- eigen_centrality(g.adj)$vector #calculo da centralidade de autovetor
  eigglist[[i]] <- eigg
}

head(wdblist[[1]]) #dados financeiros por ano
oplist #opções por ano
corlist #lista de correlação por ano
str(matcor) #matriz de correlação completa
frlist #fatores de relevancia por ano
glist #lista de grafos
strglist #centralidade strength por ano
str(adjlist) #matriz de adjacencia por ano
gadjlist #lista de grafos da matriz de adjacencia
dglist #centralidade de grau por ano
eigglist #centralidade de autovetor por ano

remove(i)
remove(wdblist)
remove(oplist)
remove(corlist)
remove(matcor)
remove(frlist)
remove(glist)
remove(strglist)
remove(adjlist)
remove(gadjlist)
remove(dglist)
remove(eigglist)

#dados para tabela centralidade de força

lapply(strglist, summary)
tab <- lapply(strglist, sort, decreasing=TRUE)
tab <- lapply(tab, data.frame)
tab <- lapply(tab, tibble::rownames_to_column, "Ações")
tab <- lapply(tab, setNames, c("Ações","Centralidade strength"))
tab
x <- c("2016","2017","2018","2019","2020","2021")
colnames <- c("Ações", paste("Centralidade de Força",x))
for (i in seq_along(tab)){
  colnames(tab[[i]]) <- c("Ações", paste("Centralidade de Força",x[i]))
}
tab <- full_join(full_join(full_join(full_join(full_join(tab[[1]],tab[[2]]),tab[[3]])
                              ,tab[[4]])
                    ,tab[[5]])
          ,tab[[6]])

tab <- print(tab, row.names=FALSE, digits = 2)

#dados para tabela centralidade de grau

lapply(dglist, summary)
tab <- lapply(dglist, sort, decreasing=TRUE)
tab <- lapply(tab, data.frame)
tab <- lapply(tab, tibble::rownames_to_column, "Ações")
tab <- lapply(tab, setNames, c("Ações","Centralidade de grau"))

x <- c("2016","2017","2018","2019","2020","2021")
colnames <- c("Ações", paste("Centralidade de Grau",x))
for (i in seq_along(tab)){
  colnames(tab[[i]]) <- c("Ações", paste("Centralidade de Grau",x[i]))
}
tab <- full_join(full_join(full_join(full_join(full_join(tab[[1]],tab[[2]]),tab[[3]])
                                     ,tab[[4]])
                           ,tab[[5]])
                 ,tab[[6]])

tab <- print(tab, row.names=FALSE, digits = 2)

#dados para tabela centralidade de autovetor

lapply(eigglist, summary)
tab <- lapply(eigglist, sort, decreasing=TRUE)
tab <- lapply(tab, data.frame)
tab <- lapply(tab, tibble::rownames_to_column, "Ações")
tab <- lapply(tab, setNames, c("Ações","Centralidade de Autovetor"))

x <- c("2016","2017","2018","2019","2020","2021")
colnames <- c("Ações", paste("Centralidade de Autovetor",x))
for (i in seq_along(tab)){
  colnames(tab[[i]]) <- c("Ações", paste("Centralidade de Autovetor",x[i]))
}
tab <- full_join(full_join(full_join(full_join(full_join(tab[[1]],tab[[2]]),tab[[3]])
                                     ,tab[[4]])
                           ,tab[[5]])
                 ,tab[[6]])

tab <- print(tab, row.names=FALSE, digits = 2)
