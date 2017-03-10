# g1 and g2 are igraphs
# gSumm is a vector of strings corresponding to graph summarization methods
  ### cen_deg = Centrality (degree)
  ### cen_clo = Centrality (closeness)
  ### cen_bet = Centrality (betweenness)
  ### ast = Assortativity
  ### com_rw = Community (random walk)
  ### com_im = Community (infomap)
  ### com_bet = Community (betweenness)
  ### dis = distance matrix
  ### eco = Edge connectivity
  ### edh = Edge density histogram
# distf is the desired difference function

GC_engine <- function(g1, g2, gSumm, distf = "euclidean", ...){
  
  stopifnot(igraph::is_igraph(g1), igraph::is_igraph(g2), 
            igraph::gorder(g1) == igraph::gorder(g2),!is.null(gSumm))
  
  diff <- rep(0,length(gSumm))
  names(diff) <- rep("",length(gSumm))
  i <- 1
  
  # Centrality (degree)
  if ("cen_deg" %in% gSumm){
    a <- igraph::centr_degree(g1)$centralization
    b <- igraph::centr_degree(g2)$centralization
    
    diff[i] <- dist_engine(a,b,distf)
    names(diff)[i] <- "cen_deg"
    i <- i + 1
  }
  
  # Centrality (closeness)
  if ("cen_clo" %in% gSumm){
    a <- igraph::centr_clo(g1)$centralization
    b <- igraph::centr_clo(g2)$centralization
    
    diff[i] <- dist_engine(a,b,distf)
    names(diff)[i] <- "cen_clo"
    i <- i + 1
  }
  
  # Centrality (betweenness)
  if ("cen_bet" %in% gSumm){
    a <- igraph::centr_betw(g1)$centralization
    b <- igraph::centr_betw(g2)$centralization
    
    diff[i] <- dist_engine(a,b,distf)
    names(diff)[i] <- "cen_bet"
    i <- i + 1
  }
  
  # Assortativity
  if ("ast" %in% gSumm){
    a <- igraph::assortativity_degree(g1)
    b <- igraph::assortativity_degree(g2)
    
    if (is.nan(a)) a <- 0
    if (is.nan(b)) b <- 0
    
    diff[i] <- dist_engine(a,b,distf)
    names(diff)[i] <- "ast"
    i <- i + 1
  }
  
  # Community (random walk)
  if ("com_rw" %in% gSumm){
    a <- igraph::membership(igraph::cluster_walktrap(g1,steps=igraph::gorder(g1)/2))
    b <- igraph::membership(igraph::cluster_walktrap(g2,steps=igraph::gorder(g2)/2))

    diff[i] <- dist_engine(a,b,dist="jaccard")
    names(diff)[i] <- "com_rw"
    i <- i + 1
  }
  
  # Community (infomap)
  if ("com_im" %in% gSumm){
    a <- igraph::membership(igraph::cluster_infomap(g1))
    b <- igraph::membership(igraph::cluster_infomap(g2))
    
    diff[i] <- dist_engine(a,b,dist="jaccard")
    names(diff)[i] <- "com_im"
    i <- i + 1
  }
  
  # Community (betweenness)
  if ("com_bet" %in% gSumm){
    a <- igraph::membership(igraph::cluster_edge_betweenness(g1))
    b <- igraph::membership(igraph::cluster_edge_betweenness(g2))
    
    diff[i] <- dist_engine(a,b,dist="jaccard")
    names(diff)[i] <- "com_bet"
    i <- i + 1
  }
  
  # distance matrix 
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
    
    diff[i] <- dist_engine(a,b,distf)
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
    
    diff[i] <- dist_engine(a,b,distf)
    names(diff)[i] <- "eco"
    i <- i + 1
  }
  
  # Edge density histogram
  if ("edh" %in% gSumm){
    # histograms should be on the same scale. 
    # Use Freedman-Diaconis rule to determine bin width
    dd <- max(igraph::degree(g1),igraph::degree(g2))
    bw <- 2 * stats::IQR(igraph::degree(g1)) / igraph::gorder(g1)^(1/3)
    a <- graphics::hist(igraph::degree(g1),plot=FALSE,breaks=seq(0,dd+bw,by=bw))$counts / igraph::gorder(g1)
    b <- graphics::hist(igraph::degree(g2),plot=FALSE,breaks=seq(0,dd+bw,by=bw))$counts / igraph::gorder(g2)
    
    diff[i] <- dist_engine(a,b,distf)
    names(diff)[i] <- "edh"
    i <- i + 1
  }
  
  diff
}