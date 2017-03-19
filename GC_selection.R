# Search for the "most similar" graph pair
# (characterized by having the lowest differences across the board)
#
# gc is a list of the differences among various graph pairs
# base is the index with which to start the search

GC_selection <- function(gc, base = 1){
  
  stopifnot(!is.null(gc))
  
  idx <- base
  old_idx <- 0
  while (old_idx != idx){
    old_idx <- idx
    idx <- best_c(gc,idx)
    #print(paste(old_idx,idx))
  }
  idx
}

# Select the next idx candidate given current best candidate
best_c <- function(gc, cand){
  
  for (i in 1:length(gc)){
    if (i != cand){
      if (sum(gc[[cand]] < gc[[i]]) < sum(gc[[cand]] > gc[[i]])){
        return( i )
      } else if (sum(gc[[cand]] < gc[[i]]) > sum(gc[[cand]] > gc[[i]])){
        # do nothing since the current candidate is better
      } else{
        return( sample(c(cand,i),1) )
      }
    }
  }
  return( cand )  
}