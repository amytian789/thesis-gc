dist_engine <- function(a,b,dist = "EUclidean", ...) {
  switch(dist,
         Euclidean=dist_euc(a,b)
         )
}

dist_euc <- function(a,b) {
  sqrt( sum( mapply( function(x,y) (x-y)^2, a, b)))
}