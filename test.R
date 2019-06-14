# script to test a scenario with written functions
# import libraries
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
# source custom functions
# source("counting.R")
# source("objective.R")
# source("gen_tables.R")
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
# baselines <- gen_tables(new_tables, f_tables)
# calculate the objective function
# obj <- calc_objective(baselines[[1]], baselines[[2]])
# print(obj)

# run the algorithm for one iteration
for (i in 1:1000){
    results <- random_descent(inp, tables, conditions, f_tables)
    inp <- results[[1]]
    tables <- results[[2]]
}