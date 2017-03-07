# g1 and g2 are igraphs
# gSumm is a vector of strings corresponding to graph summarization methods
  ### cen_clo = Centrality (closeness)
  ### ast = Assortativity
  ### com = Community (infomap / random walk)
  ### dis = Distance matrix
  ### eco = Edge connectivity
  ### edh = Edge density histogram
# dist is the desired difference function
GC_engine <- function(g1, g2, gSumm, dist = "Euclidean", ...){
  
  stopifnot(igraph::is_igraph(g1), igraph::is_igraph(g2), is.null(gSumm))
  stopifnot(vcount(g1) == vcount(g2))
  
  diff <- rep(0,length(gSumm))
  names(diff) <- rep("",length(gSumm))
  i <- 1
  
  # Centrality (closeness)
  if ("cen_clo" %in% gSumm){
    a <- igraph::centr_clo(g1)$centralization
    b <- igraph::centr_clo(g2)$centralization
    
    diff[i] <- dist_engine(a,b,dist)
    names(diff)[i] <- "cen"
    i <- i + 1
  }
  
  # Assortativity
  if ("ast" %in% gSumm){
    a <- igraph::assortativity_degree(g1)
    b <- igraph::assortativity_degree(g2)
    
    diff[i] <- dist_engine(a,b,dist)
    names(diff)[i] <- "ast"
    i <- i + 1
  }
  
  # Community (random walk? or infomap)
  if ("com" %in% gSumm){
    diff[i] <- dist_engine(a,b,dist)
    names(diff)[i] <- "com"
    i <- i + 1
  }
  
  # Distance matrix 
  if ("dis" %in% gSumm){
    a <- distances(g1)
    b <- distances(g2)
    
    # change from matrix -> vector for distance computation (ordered by column)
    # keep only 1 side of the matrix (both sides are the same)
    # don't keep the values in the middle (since it's the same node)
    a[a == Inf] <- 0
    a <- a[upper.tri(a)] / (igraph::gorder(g1)-1)
    b[b == Inf] <- 0
    b <- b[upper.tri(b)] / (igraph::gorder(g2)-1)
    
    diff[i] <- dist_engine(a,b,dist)
    names(diff)[i] <- "dis"
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
    names(diff)[i] <- "eco"
    i <- i + 1
  }
  
  # Edge density histogram
  if ("edh" %in% gSumm){
    # histograms should be on the same scale. 
    # Use Freedman-Diaconis rule to determine bin width
    bw <- 2 * stats::IQR(igraph::degree(g1)) / igraph::gorder(g1)^(1/3)
    a <- graphics::hist(igraph::degree(g1),plot=FALSE,breaks=seq(0,gorder(g1),by=bw))$counts / igraph::gorder(g1)
    b <- graphics::hist(igraph::degree(g2),plot=FALSE,breaks=seq(0,gorder(g2),by=bw))$counts / igraph::gorder(g2)
    
    diff[i] <- dist_engine(a,b,dist)
    names(diff)[i] <- "edh"
    i <- i + 1
  }
  
  diff
}