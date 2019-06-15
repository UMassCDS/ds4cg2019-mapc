# This script is meant to execute the randomized descent algorithm for finding weights that match the specified targets

library(data.table)
library(dplyr)

source("objective.R")

save_list <- readRDS("savefile.RData")
num_iters <- 350
update_factor <- 0.01

file_name <- save_list[["file_name"]]   # Input file name (PUMS dataset)
target_var <- save_list[["target_var"]] # Target variable name

data <- fread(file=file_name) # Read the input file
weights <- as.numeric(data[[target_var]]) # Extract the weights as a numeric vector

num_tables <- length(save_list) - 2

ids <- list()
baselines <- list()
targets <- list()


for(t in seq(num_tables)){
  table <- save_list[[t]]
  
  target_data <- fread(file=paste(table[[1]], ".csv", sep=""))
  
  baselines[[t]] <- as.numeric(target_data[["BASELINE"]])
  targets[[t]] <- as.numeric(target_data[["TARGET"]])
  ids[[t]] <- save_list[[t]][[7]]
}

of_value <- calc_objective(targets, baselines)


rseq <- sample(1:nrow(data), num_iters, replace=TRUE)


for(r in rseq){
  
  of_new <- of_value
  w_delta <- update_factor * weights[r] # How much to increase the current weight by
  
  for(t in seq(num_tables)){
    
    id <- ids[[t]][r]
    if(id == 0){ next }
    
    target_val <- targets[[t]][id]
    baseline_old <- baselines[[t]][id]
    baseline_new <- baseline_old + w_delta
    
    # Computing the updated value of the objective function using only the affected cell of the target matrix
    of_new <- of_new - (target_val - baseline_old) ^ 2 + (target_val - baseline_new) ^ 2
    
  }
  
  if(of_new < of_value){  # If the objective function value improves, accept the change
    weights[r] <- weights[r] + w_delta
    for(t in seq(num_tables)){
      id <- ids[[t]][r]
      if(id > 0){ baselines[[t]][id] <- baselines[[t]][id] + w_delta }
    }
    of_value <- of_new
    next
  }
  
  # If the objective function does not improve, then the following code gets executed
  
  of_new <- of_value
  w_delta <- -w_delta   # How much to decrease the current weight by
  
  for(t in seq(num_tables)){
    
    id <- ids[[t]][r]
    if(id == 0){ next }
    
    target_val <- targets[[t]][id]
    baseline_old <- baselines[[t]][id]
    baseline_new <- baseline_old + w_delta
    
    # Computing the updated value of the objective function using only the affected cell of the target matrix
    of_new <- of_new - (target_val - baseline_old) ^ 2 + (target_val - baseline_new) ^ 2
    
  }
  
  if(of_new < of_value){  # If the objective function value improves, accept the change
    weights[r] <- weights[r] + w_delta
    for(t in seq(num_tables)){
      id <- ids[[t]][r]
      if(id > 0){ baselines[[t]][id] <- baselines[[t]][id] + w_delta }
    }
    of_value <- of_new
  }
  
  print(of_value) # Print the value of the objective function at the end of every iteration
}

w <- data.frame(weights)

colnames(w) <- c(target_var)

fwrite(w, file="weights.txt")