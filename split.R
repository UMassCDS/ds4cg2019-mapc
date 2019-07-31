library(dplyr)
library(data.table)

options(digits=20)

source("objective.R")

split_hh <- function(inp, cond, num_iter, wflag){
    # get the number of tables and blocks
    n_blocks <- length(cond) - 2
    n_tables <- list()
    # get input file name
    inp_file <- cond[["file_name"]]
    # get the target variable
    target_var <- list()
    # get the initial weights as a numeric vector
    weights <- list()
    # get the data for each block
    for (b in seq(n_blocks)){
        # get the number of tables
        n_tables[[b]] <- cond[[b]][["num_tables"]]
        # get the target variable
        target_var[[b]] <- cond[[b]][["target_var"]]
        # get the weights
        weights[[cond[[b]][["target_var"]]]] <- as.numeric(inp[[target_var[[b]]]])
    }
    # init the ids, baselines and targets
    ids <- list()
    baselines <- list()
    targets <- list()

    # iterate through all the tables
    for (b in seq(n_blocks)){
        t_baselines <- list()
        t_targets <- list()
        t_ids <- list()
        for (t in seq(n_tables[[b]])){
            table <- cond[[b]][[t]]
            data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
            # get the ids, baseline and target values
            t_baselines[[t]] <- as.double(data[["BASELINE"]]) 
            t_targets[[t]] <- as.double(data[["TARGET"]]) 
            t_ids[[t]] <- cond[[b]][[t]][[7]]
            # add a new column containing the intermediate weights 
            data <- mutate(data, INTER=t_baselines[[t]])
            data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
        }
        # save the baselines, targets and ids
        baselines[[b]] <- t_baselines
        targets[[b]] <- t_targets
        ids[[b]] <- t_ids
    }
    # calculate the initial objective function
    of_val <- calc_objective(targets, baselines)
    print(paste("init ofval: ", of_val))
    # run the algorithm for num_iter steps
    for (iter in seq(num_iter)){
        # store the previous OFVal
        prev_val <- of_val
        start_time <- Sys.time()
        # get a random integer ordering such that main person of the HH is selected
        r_int <- which(inp$SPORDER == 1)
        # iterate through the rows
        for (r in r_int){
            # store the objective function value
            of_new <- of_val
            # get the list of people in the HH
            child <- cond[["children"]][[r]]
        }
    }



    quit()
}