# script to test a scenario with written functions
# import libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))

# source custom functions
source("split.R")

# read input and base matrix files
# read the input file
inp <- data.table::fread(file="extract_for_test.csv")
# read the savefile RData object
conditions <- read_rds("savefilehh.RData")
# set the number of iterations
num_iters <- 350
# set the write flag
write_flag <- TRUE

# run the algorithm
geog_split <- split_hh(inp, conditions, num_iters, write_flag)
# write the weights to file
data.table::fwrite(geog_split, file="split_final.csv")
write.csv(geog_split, file="split_final.csv")
