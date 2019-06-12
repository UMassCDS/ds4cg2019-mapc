# Saurabh Shirodkar and Pratheek Mallya
#
# This script is meant to process MAPC reweighting config files. Please see the README for the format of the config file.

library(rjson)
library(dplyr)
library(data.table)

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

config <- fromJSON(file=config_file)

num_tables <- length(config[["tables"]])

file_name <- config[["file_name"]]
target_var <- config[["target_var"]]

data <- fread(file=file_name)


save_list <- list()

for(t in seq(num_tables)){
  table <- config[["tables"]][[t]]
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
  
  d <- data %>%
    select(c(unlist(var_names), target_var)) %>%
    as.matrix()
  
  ids <- apply(d, 1, function(row){ # For each row, find which index of the target matrix the weight is to be added to
    for(i in seq(num_dims)){
      for(j in seq(dim_vec[i])){
        truth_val <- funcs[[i]][[j]](row[i])
        if(!is.na(truth_val) & truth_val == TRUE){
          ivec[i] <- j
          break
        }
        else if(j == dim_vec[i]){
          return(0)
        }
      }
    }
    index <- coord2index(ivec)
    return(index)
  })
  
  num_cells <- prod(dim_vec)  # Number of cells in the target matrix
  target_vector <- vector(mode="numeric", length=num_cells)
  
  for(i in seq(length(ids))){
    id <- ids[i]
    if(id > 0){
      target_vector[id] <- target_vector[id] + d[i, ncol(d)]  # Last column of d is target variable
    }
  }
  
  tf <- paste(c(name, ".csv"), collapse="") # Target file name
  
  header <- paste(unlist(var_names), collapse=",")
  header <- paste(c(header, "BASELINE", "TARGET"), collapse=",")
  write(header, file=tf, append=FALSE)
  
  for(n in seq(num_cells)){
    ivec <- index2coord(n)
    row <- vector(mode="character", length=num_dims)
    for(i in seq(num_dims)){
      row[i] <- conds[[i]][[ivec[i]]]
    }
    row <- paste(c(row, target_vector[n], target_vector[n]), collapse=",")
    write(row, file=tf, append=TRUE)
  }
  
  new_list <- list()
  new_list[[1]] <- name
  new_list[[2]] <- dim_vec
  new_list[[3]] <- var_names
  new_list[[4]] <- var_types
  new_list[[5]] <- conds
  new_list[[6]] <- funcs
  new_list[[7]] <- ids    # Indices of the input CSV rows into the target vector
    
  save_list[[t]] <- new_list
}

save_list[["file_name"]] <- file_name
save_list[["target_var"]] <- target_var

saveRDS(save_list, file="savefile.RData")