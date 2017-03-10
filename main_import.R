setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-gc/")
source("GC_engine.R")
source("dist_engine.R")
source("GC_selection.R")

library(xlsx)
library(igraph)
library(clusteval)





################################## Read in data.

g <- length(xlsx::getSheets(loadWorkbook("data.xlsx")))
if (g %% 2 != 0) {
  stop("The number of sheets must be even. For any graph pair, 
       each graph's sheet must be next to the other's.")
}
data <- vector("list",length(g))
for (i in 1:g) {
  # First number = number of nodes in graph
  data[[i]] <- as.vector(t( xlsx::read.xlsx("data.xlsx", sheetIndex = i, header = FALSE) ))[-1]
}





################################## Compute differences for all given graph pairs

# Setting parameters
gSumm <- c("cen_deg","cen_clo","cen_bet","ast",
             "com_rw","com_im","com_bet","dis","eco","edh")
distf <- "l2"

gc <- vector("list",length(g/2))
for (i in seq(1,g,2)) {
  # g1 and g2 must have the same number of nodes!
  if (data[[i]][1] != data[[i+1]][1]) {
    stop(paste("g1 and g2 must have the same # of nodes. 
               Error in sheets",i,"and",i+1))
  }

  # Create g1 and g2 as igraphs
  g1 <- igraph::make_empty_graph(n = data[[i]][1], directed = FALSE)
  g1 <- igraph::add_edges(g1, data[[i]][-1])
  g2 <- igraph::make_empty_graph(n = data[[i+1]][1], directed = FALSE)
  g2 <- igraph::add_edges(g2, data[[i+1]][-1])
  
  # Call the engine to compute difference between g1 and g2
  gc[[(i+1)/2]] <- GC_engine(g1=g1,g2=g2,gSumm=gSumm,distf=distf)
}





################################## Select the most similar graph pair

idx <- GC_selection(gc = gc, base = 1)
idx





# ll <- apply(combn(seq_along(gc),2), 2, function(n) max(sum(gc[[n[1]]] < gc[[n[2]]]), sum(gc[[n[1]]] > gc[[n[2]]])))
# idx <- which(ll==max(ll))
# if (length(idx) > 1) idx <- sample(idx,1)
# n <- combn(seq_along(gc),2)[,idx] # index in list of candidate "best" pair
# if (sum(gc[[n[1]]]<gc[[n[2]]]) < sum(gc[[n[1]]]>gc[[n[2]]])){
#   n[2]
# } else if (sum(gc[[n[1]]]<gc[[n[2]]]) > sum(gc[[n[1]]]>gc[[n[2]]])) {
#   n[1]
# } else {
#   sample(n,1)
# }