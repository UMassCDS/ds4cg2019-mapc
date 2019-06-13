# script to test a scenario with written functions
# import libraries
library(data.table)
library(readr)
library(dplyr)
# source custom functions
source("counting.R")
source("objective.R")
source("base_algo.R")
# read inuput and base matrix files
inp <- data.table::fread(file="ss16pma.csv")
f_tables <- c("FirstTable.csv", "SecondTable.csv")
conditions <- read_rds("savefile.RData")
tables <- list()
for (i in seq_along(f_tables)){
    tables[[i]] <- data.table::fread(file=f_tables[[i]], fill=TRUE)
}

# generate the intermediate counts
# new_tables <- gen_counts(inp, tables, conditions)
# generate the targets and baseline values
# targets <- list()
# baselines <- list()
# for (i in seq_along(f_tables)){
#     data.table::fwrite(new_tables[[i]], file=f_tables[[i]])
#     targets[[i]] <- new_tables[[i]]$TARGET 
#     baselines[[i]] <- new_tables[[i]]$INTER

# }
# calculate the objective function
# obj <- calc_objective(targets, baselines)
# print(obj)

# run the algorithm for one iteration
random_descent(inp, tables, conditions)
