setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-gc/")
source("GC_engine.R")
source("dist_engine.R")

library(xlsx)
library(igraph)
library(clusteval) # for Jaccard index: cluster_similarity

# Read in data.
g <- 2 # number of sheets in file. Must be an even number
data <- vector("list",length(g))
for (i in 1:g) {
  # First number = number of nodes in graph
  data[[i]] <- as.vector(t( xlsx::read.xlsx("data.xlsx", sheetIndex = i, header = FALSE) ))[-1]
}

for (i in seq(1,g,2)) {
  # g1 and g2 must have the same number of nodes!
  if (data[[i]][1] != data[[i+1]][1]) {
    stop(paste("g1 and g2 must have the same # of nodes. Error in sheets",i,"and",i+1))
  }

  # create g1 and g2 as igraphs
  g1 <- igraph::make_empty_graph(n = data[[i]][1], directed = FALSE)
  g1 <- igraph::add_edges(g1, data[[i]][-1])
  g2 <- igraph::make_empty_graph(n = data[[i+1]][1], directed = FALSE)
  g2 <- igraph::add_edges(g2, data[[i+1]][-1])
  
  # call the engine to compute difference between g1 and g2
}