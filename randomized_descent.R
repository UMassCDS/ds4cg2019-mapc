# Saurabh Shirodkar and Pratheek Mallya
#
# This script is meant to execute the randomized descent algorithm for finding weights that match the specified targets

library(data.table)
library(dplyr)

source("objective.R")

save_list <- readRDS("savefile.RData")
num_iters <- 350
update_factor <- 0.01

file_name <- save_list[["file_name"]]
target_var <- save_list[["target_var"]]

data <- fread(file=file_name)
weights <- as.numeric(data[[target_var]]) # Extract the weights as a numeric vector

num_tables <- length(save_list) - 2

ids <- list()
baselines <- list()
targets <- list()
residues <- list()


for(t in seq(num_tables)){
  table <- save_list[[t]]
  
  target_data <- fread(file=paste(table[[1]], ".csv", sep=""))
  
  baselines[[t]] <- as.numeric(target_data[["BASELINE"]])
  targets[[t]] <- as.numeric(target_data[["TARGET"]])
  residues[[t]] <- (targets[[t]] - baselines[[t]]) ^ 2
  ids[[t]] <- save_list[[t]][[7]]
}

of_value <- calc_objective(targets, baselines)



for(i in seq(num_iters)){
  
  rseq <- sample(1:nrow(data), nrow(data))  # Pick a random order of rows for the iteration
  
  for(r in rseq){
    
    of_new <- of_value
    w_delta <- update_factor * weights[r]
    
    for(t in seq(num_tables)){
      
      id <- ids[[t]][r]
      if(id == 0){ next }
      
      target_val <- targets[[t]][id]
      baseline_old <- baselines[[t]][id]
      baseline_new <- baseline_old + w_delta
      
      of_new <- of_new - (target_val - baseline_old) ^ 2 + (target_val - baseline_new) ^ 2
      
    }
    
    if(of_new < of_value){
      weights[r] <- weights[r] + w_delta
      for(t in seq(num_tables)){
        id <- ids[[t]][r]
        if(id > 0){ baselines[[t]][id] <- baselines[[t]][id] + w_delta }
      }
      of_value <- of_new
      next
    }
    
    of_new <- of_value
    w_delta <- -w_delta
    
    for(t in seq(num_tables)){
      
      id <- ids[[t]][r]
      if(id == 0){ next }
      
      target_val <- targets[[t]][id]
      baseline_old <- baselines[[t]][id]
      baseline_new <- baseline_old + w_delta
      
      of_new <- of_new - (target_val - baseline_old) ^ 2 + (target_val - baseline_new) ^ 2
      
    }
    
    if(of_new < of_value){
      weights[r] <- weights[r] + w_delta
      for(t in seq(num_tables)){
        id <- ids[[t]][r]
        if(id > 0){ baselines[[t]][id] <- baselines[[t]][id] + w_delta }
      }
      of_value <- of_new
    }
    
  }
  print(of_value)
}

