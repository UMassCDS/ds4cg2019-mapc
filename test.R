# script to test a scenario with written functions
# import libraries
library(data.table)
library(readr)
library(dplyr)
# source custom functions
source("counting.R")
source("objective.R")
# read inuput and base matrix files
inp <- data.table::fread(file="ss16pma.csv")
f_tables <- c("FirstTable.csv", "SecondTable.csv")
conditions <- read_rds("savefile.RData")
tables <- list()
for (i in seq_along(f_tables)){
    tables[[i]] <- data.table::fread(file=f_tables[[i]], fill=TRUE)
}

# tables[[1]]
# tables[[2]]
new_tables <- gen_counts(inp, tables, conditions)
# new_tables[[1]]
# new_tables[[2]]
targets <- list()
baselines <- list()
for (i in seq_along(f_tables)){
    data.table::fwrite(new_tables[[i]], file=f_tables[[i]])
    targets[[i]] <- new_tables[[i]]$TARGET 
    baselines[[i]] <- new_tables[[i]]$INTER

}
obj <- calc_objective(targets, baselines)
print(obj)