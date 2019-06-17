# base randomized descent algorithm
# import libraries
library(dplyr)
library(data.table)
# import custom functions
source("objective.R")

# randomized descent algoritm
# INPUTS:-
# inp: data table of input (ss16pma.csv)
# cond: data table of savefile.RData
# num_iter: number of iterations the algorithm should run
# u_factor: factor by which the weights are updated
# wflag: flag whether to write data to file
# OUTPUTS: set of updated weights 
random_descent <- function(inp, cond, num_iter, u_factor, wflag) {
    # get the number of tables
    n_tables <- length(cond) - 2
    # get input file name
    inp_file <- cond[["file_name"]]
    # get the target variable
    target_var <- cond[["target_var"]]
    # get the initial weights as a numeric vector
    weights <- as.numeric(inp[[target_var]])

    # init the ids, baselines and targets
    ids <- list()
    baselines <- list()
    targets <- list()

    # iterate through all the tables
    for (t in seq(n_tables)){
        table <- cond[[t]]
        data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
        # get the ids, baseline and target values
        baselines[[t]] <- as.numeric(data[["BASELINE"]])
        targets[[t]] <- as.numeric(data[["TARGET"]])
        ids[[t]] <- cond[[t]][[7]]
        # add a new column containing the intermediate weights 
        data <- mutate(data, INTER=baselines[[t]])
        data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
    }

    # calculate the initial objective function
    of_val <- calc_objective(targets, baselines)
    print(paste("init ofval: ", of_val))
    # run the algorithm for num_iter steps
    for (i in seq(num_iter)){
        # store the previous OFVal
        prev_val <- of_val
        start_time <- Sys.time()
        # get a random arrangement of the rows for iterating through
        r_int <- sample(nrow(inp), nrow(inp), replace=FALSE)
        # iterate through the rows
        for (r in r_int){
            # print(paste("r: ", r))
            of_new <- of_val
            # print(paste("ofnew1: ", of_new))
            w_delta <- u_factor * weights[r]
            for (t in seq(n_tables)){
                id <- ids[[t]][r]
                if (id == 0){next}
                target_val <- targets[[t]][id]
                base_old <- baselines[[t]][id]
                base_new <- base_old + w_delta
                # print(paste("base_old1: ", base_old))
                # print(paste("base_new1: ", base_new))
                of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
                # print(paste("ofnew2: ", of_new))
            }
            # check whether the new OFVal is better than the old OFVal when increasing the 
            # weight by the update factor
            if (of_new < of_val){
                # update the weights
                weights[r] <- weights[r] + w_delta
                for (t in seq(n_tables)){
                    id <- ids[[t]][r]
                    if (id > 0) {
                        baselines[[t]][id]  <- baselines[[t]][id] + w_delta
                    }
                }
                of_val <- of_new
            }
            # if not, decrease the weight by the update factor and try again
            else {
                of_new <- of_val
                for (t in seq(n_tables)){
                    id <- ids[[t]][r]
                    if (id == 0){next}
                    target_val <- targets[[t]][id]
                    base_old <- baselines[[t]][id]
                    base_new <- base_old - w_delta
                    # print(paste("base_old2: ", base_old))
                    # print(paste("base_new2: ", base_new))
                    of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
                    # print(paste("ofnew3: ", of_new))
                }
                # check whether the OFVal has improved
                if (of_new < of_val){
                    weights[r] <- weights[r] - w_delta
                    for (t in seq(n_tables)){
                        id <- ids[[t]][r]
                        if (id > 0){
                            baselines[[t]][id]  <- baselines[[t]][id] - w_delta
                        }
                    }
                    of_val <- of_new
                }
            }
        }
        # write the data to file
        if (wflag) {
            for (t in seq(n_tables)){
                table <- cond[[t]]
                data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
                data <- mutate(data, INTER=baselines[[t]])
                data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
            }
        }
        print(paste("OFVal: ", of_val))
        new_val <- of_val
        
        # check for the early exit condition
        if (abs(new_val - prev_val) < 0.000000001) {
            print("Early Exit")
            print(paste("Number of Iterations: ", i))
             break
        }
        prev_val <- new_val
        end_time <- Sys.time()
        time_taken <- end_time - start_time
        print(paste("Iteration Time: ", time_taken))

    }
    # return the updated weights
    return(weights)
}