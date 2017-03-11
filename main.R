#setwd("C:/Users/amyti/Documents/Amy - COLLEGE/THESIS/thesis-gc/")
source("GC_engine.R")
source("dist_engine.R")
source("GC_selection.R")

library(igraph)
library(clusteval)

################################## Random sample given PDF

randomdraw <- function(n, prob){
  if(sum(prob) != 1 | length(which(prob<0)) > 0) stop("Probability must be between 0 and 1")
  
  runningsum <- 0
  s <- runif(n)
  for (i in 1:n){
    for (j in 1:length(prob)){
      runningsum <- runningsum + prob[j]
      if (s[i] < runningsum){
        s[i] <- j
        break
      } 
    }
  }
  s
}

################################## Run simulations

# Create base graph
set.seed(10)
bg <- igraph::sample_gnm(20, 100)
bg_e <- igraph::as_edgelist(bg)

# Setting parameters
gSumm <- c("cen_deg","cen_clo","cen_bet","ast",
           "com_rw","com_im","com_bet","dis","eco","edh")
distf <- "l2"

msg <- rep(0,1000)
for (i in 1:1000) {
  # g1: randomly swap 20 edges. Weight of each node is proportional to its degree
  g1 <- bg 
  set.seed(i)
  for (j in 1:20) {
    idx <- sample(igraph::as_edgelist(g1),1)
    g1 <- igraph::delete_edges(g1,idx)
    prob <- igraph::degree(g1) / sum(igraph::degree(g1))
    nv <- randomdraw(2, prob)
    g1 <- igraph::add_edges(g1, nv)
  }
  
  # g2: randomly swap 100 edges. Weight of each node is proportional to its degree
  g2 <- bg
  set.seed(i)
  for (j in 1:100) {
    idx <- sample(igraph::as_edgelist(g2),1)
    g2 <- igraph::delete_edges(g2,idx)
    prob <- igraph::degree(g2) / sum(igraph::degree(g2))
    nv <- randomdraw(2, prob)
    g2 <- igraph::add_edges(g2, nv)
  }
  
  # Compute difference between (bg,g1), (bg,g2)
  gc <- vector("list",2)
  gc[[1]] <- GC_engine(g1=bg,g2=g1,gSumm=gSumm,distf=distf)
  gc[[2]] <- GC_engine(g1=bg,g2=g2,gSumm=gSumm,distf=distf)
  
  # Select the most similar graph pair
  msg[i] <- GC_selection(gc = gc, base = 1)
}
p1 <- length(which(msg == 1)) / length(msg)
p2 <- length(which(msg == 2)) / length(msg)
cat("Prob. of selecting (bg,g1):",p1,"\nProb. of selecting (bg,g2):", p2)

