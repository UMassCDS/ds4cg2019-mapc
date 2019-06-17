# script to test a scenario with written functions
# import libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# source custom functions
source("counting.R")
source("base_algo.R")

# read inuput and base matrix files
inp <- data.table::fread(file="ss16pma.csv")
conditions <- read_rds("savefile.RData")
num_iters <- 15
update_factor <- 0.01
write_flag <- TRUE

# run the algorithm for one iteration
# small changes causing massive change in OFValue, why?
weights <- random_descent(inp, conditions, num_iters, update_factor, write_flag)
weights <- data.table(weights)
data.table::fwrite(weights, file="weights_final.csv")
