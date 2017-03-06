# g1 and g2 are graphs (an object)
# gSumm is a vector of strings corresponding to graph summarization methods
  ### cen = centrality
  ### ast = assortativity
  ### com = community
  ### dis = distance matrix
  ### eco = edge connectivity
  ### edh = edge density histogram
# dist is the desired difference function
GC_engine <- function(g1, g2, gSumm = NULL, dist = "Euclidean", ...){
  
#   stopifnot(nrow(X) == length(y), is.matrix(X), is.factor(y), length(levels(y)) == 2)
#   idx <- which(is.na(y_unlabeled))
#   stopifnot(length(idx) > 0, all(y[-idx] == y_unlabeled[-idx]), length(y) == length(y_unlabeled),
#             is.factor(y_unlabeled))
  
  diff <- rep(0,length(gSumm))
  i <- 1
  
  # Centrality
  if ("cen" %in% gSumm){
    diff[i] <- function(g1,g2,dist,...)
    i <- i + 1
  }
  
  # Assortativity
  if ("ast" %in% gSumm){
    diff[i] <- function(g1,g2,dist,...)
    i <- i + 1
  }
  
  # Community
  if ("com" %in% gSumm){
    diff[i] <- function(g1,g2,dist,...)
    i <- i + 1
  }
  
  # Distance matrix 
  if ("dis" %in% gSumm){
    diff[i] <- function(g1,g2,dist,...)
    i <- i + 1
  }
  
  # Edge connectivity 
  if ("eco" %in% gSumm){
    if (min(igraph::degree(g1)) != 0){
      a <- igraph::edge_connectivity(g1) / min(igraph::degree(g1))
    } else a <- 0
    if (min(igraph::degree(g2)) != 0){
      b <- igraph::edge_connectivity(g2) / min(igraph::degree(g2))
    } else b <- 0
    
    diff[i] <- dist_engine(a,b,dist)
    i <- i + 1
  }
  
  # Edge density histogram
  if ("edh" %in% gSumm){
    diff[i] <- function(g1,g2,dist,...)
    i <- i + 1
  }
  
  diff
}