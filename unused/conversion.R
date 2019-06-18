get_hvec <- function(dim_vec){ # Get the helping vector for moving between coordinates and indices
  num_dims <- length(dim_vec)
  hvec <- vector(mode="numeric", length=num_dims)
  for(i in seq(num_dims)){
    if(i < num_dims){
      hvec[i] <- prod(dim_vec[(i+1):num_dims])  # Component i of hvec denotes after how many indices that dimension changes value(condition)
    }
    else{   # i == num_dims
      hvec[i] <- 1
    }
  }
  return(hvec)
}

index2coord <- function(index, dim_vec, hvec){
  num_dims <- length(dim_vec)
  ivec <- vector(mode="numeric", length=num_dims)
  for(i in seq(num_dims)){
    ivec[i] <- ceiling(index/hvec[i])
    if(ivec[i] == 0){ ivec[i] <- dim_vec[i] } # This needs to be done because vectors are 1-indexed in R
    index <- index %% hvec[i]
  }
  return(ivec)
}

coord2index <- function(ivec, dim_vec, hvec){
  num_dims <- length(dim_vec)
  index <- 1
  for(i in seq(num_dims)){
    if(ivec[i] > 1){
      index <- index + (ivec[i]-1) * hvec[i]
    }
  }
  return(index)
}