# script to test a scenario with written functions
# import libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# source custom functions
source("algo_hh_optim.R")

# read input and base matrix files
# read the input file
inp <- data.table::fread(file="POP2016_temp.csv")
# read the savefile RData object
conditions <- read_rds("savefilehh.RData")
# set the number of iterations
num_iters <- 3
# set the update factor
update_factor <- 0.05
# set the write flag
write_flag <- TRUE

# run the algorithm
weights <- random_descent_hh(inp, conditions, num_iters, update_factor, write_flag)
# write the weights to file
data.table::fwrite(weights, file="weights_final.csv")
