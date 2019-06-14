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
num_iters <- 1
update_factor <- 0.01
write_flag <- FALSE

# run the algorithm for one iteration
weights <- random_descent(inp, conditions, num_iters, update_factor, write_flag)