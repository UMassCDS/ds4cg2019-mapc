# Saurabh Shirodkar and Pratheek Mallya
#
# This script is meant to process MAPC reweighting config files. Please see the README for the format of the config file.

library(rjson)

generator <- function(condition){ # Need to do this because R.
  force(condition)
  expr <- parse(text=condition)
  c <- as.character(expr)
  f <- function(x){
    return(eval(expr))
  }
  ret <- list()
  ret[[1]] <- c
  ret[[2]] <- f
  return(ret)
}

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


config_file <- "reweighting_config.json"

data <- fromJSON(file=config_file)

num_tables <- length(data[["tables"]])

save_list <- list()

for(t in seq(num_tables)){
  table <- data[["tables"]][[t]]
  name <- table[["name"]]
  dims <- table[["dims"]]
  num_dims <- length(dims)
  dim_vec <- vector(mode="numeric", length=num_dims)  # Vector that will contain the table dimensions
  var_names <- list()  # List that will contain the variable name corresponding to each dimension
  var_types <- list()  # List that will contain the variable type corresponding to each dimension
  conds <- list()   # List that will contain the conditions as strings/characters
  funcs <- list()   # List that will contain the conditions as actual functions on the variable 'x'
  
  for(i in seq(num_dims)){
    dim <- dims[[i]]
    var_names[[i]] <- dim[["var"]]
    var_types[[i]] <- dim[["type"]]
    conditions <- dim[["conditions"]]
    num_conditions <- length(conditions)
    dim_vec[i] <- num_conditions
    conds[[i]] <- list()  # Create a new list for the current dimension
    funcs[[i]] <- list()  # Create a new list for the current dimension
    
    for(j in seq(num_conditions)){
      ret <- generator(conditions[[j]])
      conds[[i]][[j]] <- ret[[1]]
      funcs[[i]][[j]] <- ret[[2]]
    }
  }
  
  # The following code will create a CSV file for the current table
  ivec <- vector(mode="numeric", length=num_dims) # Vector representing matrix coordinates for a number.
  
  hvec <- get_hvec(dim_vec) # Helping vector to convert a number to matrix coordinates.
  
  # The following 2 functions are for switching between indices and coordinates
  # These functions will make use of dim_vec, ivec, hvec and num_dims objects created above
  
  index2coord <- function(index){ # Takes as input an index and outputs the coordinates as a vector
    for(i in seq(num_dims)){
      ivec[i] <- ceiling(index/hvec[i])
      if(ivec[i] == 0){ ivec[i] <- dim_vec[i] } # This needs to be done because vectors are 1-indexed in R
      index <- index %% hvec[i]
    }
    return(ivec)
  }
  
  coord2index <- function(ivec){  # Takes as input the coordinates as a vector and outputs an index
    index <- 1
    for(i in seq(num_dims)){
      if(ivec[i] > 1){
        index <- index + (ivec[i]-1) * hvec[i]
      }
    }
    return(index)
  }
  
  total_rows <- prod(dim_vec)
  
  tf <- paste(c(name, ".csv"), collapse="") # Target file name
  
  header <- paste(unlist(var_names), collapse=",")
  header <- paste(c(header, "TARGET"), collapse=",")
  write(header, file=tf, append=FALSE)
  
  for(n in seq(total_rows)){
    ivec <- index2coord(n)
    row <- vector(mode="character", length=num_dims)
    for(i in seq(num_dims)){
      row[i] <- conds[[i]][[ivec[i]]]
    }
    row <- paste(row, collapse=",")
    write(row, file=tf, append=TRUE)
  }
  
  new_list <- list()
  new_list[[1]] <- name
  new_list[[2]] <- dim_vec
  new_list[[3]] <- var_names
  new_list[[4]] <- var_types
  new_list[[5]] <- conds
  new_list[[6]] <- funcs
  
  save_list[[t]] <- new_list
}

saveRDS(save_list, file="savefile.RData")