# baseline randomized descent algorithm
# import libraries
library(dplyr)
# import custom functions
source("objective.R")
source("counting.R")
source("gen_tables.R")
# randomized descent algoritm
random_descent <- function(inp, tables, conditions, f_tables) {
    # source <- inp
    # cond <- conditions
    # print(paste("rows:", nrow(inp)))
    # print(paste("cols:", ncol(inp)))
    r_int <- sample(1:nrow(inp), 1)
    # print(paste("row:", r_int))
    # increase the weight by 1% 
    # print(paste("PWGTP: ", inp[r_int, ]$PWGTP))
    new_tables <- gen_counts(inp, tables, conditions)
    baselines <- gen_tables(new_tables, f_tables)
    obj <- calc_objective(baselines[[1]], baselines[[2]])
    # print(paste("OFVal:", obj))
    new_wt <- inp[r_int, ]$PWGTP * 1.1
    # save the updated weight in the input
    inp[r_int, ]$PWGTP <- floor(new_wt)
    newer_tables <- gen_counts(inp, tables, conditions)
    new_baselines <- gen_tables(newer_tables, f_tables)
    new_obj <- calc_objective(new_baselines[[1]], new_baselines[[2]])
    if (obj < new_obj){
        new_wt <- inp[r_int, ]$PWGTP * 0.9
        inp[r_int, ]$PWGTP <- floor(new_wt)
        newer_tables <- gen_counts(inp, tables, conditions)
        new_baselines <- gen_tables(newer_tables, f_tables)
        new_obj <- calc_objective(new_baselines[[1]], new_baselines[[2]])
    }
    # print(paste("Updated PWGTP:", new_wt))
    print(paste("New OFVal:", new_obj))
    temp <- list()
    temp[[1]] <- inp 
    temp[[2]] <- tables 
    return(temp)
}