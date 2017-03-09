# Engine to call various distance computation methods
dist_engine <- function(a,b,dist = "euclidean", ...){
  switch(dist,
         euclidean=dist_euc(a,b),
         l1=dist_l1(a,b),
         l2=dist_l2(a,b),
         jaccard=dist_jac(a,b)
         )
}

# Euclidean distance
dist_euc <- function(a,b){
  sqrt( sum( mapply( function(x,y) (x-y)^2, a, b)))
}

# L1: Sum of absolute differences
dist_l1 <- function(a,b){
  sum ( mapply ( function(x,y) abs(x-y), a, b))
}

# L2: Sum of squared differences
dist_l2 <- function(a,b){
  sum ( mapply ( function(x,y) (x-y)^2, a, b))
}

# Jaccard distance
dist_jac <- function(a,b){
  1-clusteval::cluster_similarity(a,b,similarity="jaccard")
}