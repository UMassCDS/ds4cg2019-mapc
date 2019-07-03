# script to test a scenario with written functions
# import libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# source custom functions
source("base_algo_hh.R")

# read inuput and base matrix files
inp <- data.table::fread(file="POP2016.csv")
conditions <- read_rds("savefilehh.RData")
num_iters <- 500
update_factor <- 0.01
write_flag <- TRUE

# run the algorithm for one iteration
weights <- random_descent_hh(inp, conditions, num_iters, update_factor, write_flag)
# write the weights to file
data.table::fwrite(weights, file="weights_final.csv")
