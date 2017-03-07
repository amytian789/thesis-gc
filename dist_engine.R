dist_engine <- function(a,b,dist = "EUclidean", ...){
  switch(dist,
         Euclidean=dist_euc(a,b),
         Abs=dist_abs(a,b)
         )
}

dist_euc <- function(a,b){
  sqrt( sum( mapply( function(x,y) (x-y)^2, a, b)))
}

dist_abs <- function(a,b){
  # check if vector?!!
  sum ( mapply ( function(x,y) abs(x-y), a, b))
}