# base randomized descent algorithm with functionality for changing HH weights as well as Person weights
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
random_descent_hh <- function(inp, cond, num_iter, u_factor, wflag) {
    # get the number of tables and blocks
    n_blocks <- length(cond) - 1
    n_tables <- list()
    # get input file name
    inp_file <- cond[["file_name"]]
    # get the target variable
    target_var <- list()
    # get the initial weights as a numeric vector
    weights <- list()
    for (b in seq(n_blocks)){
        n_tables[[b]] <- cond[[b]][["num_tables"]]
        target_var[[b]] <- cond[[b]][["target_var"]]
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
            t_baselines[[t]] <- as.numeric(data[["BASELINE"]])
            t_targets[[t]] <- as.numeric(data[["TARGET"]])
            t_ids[[t]] <- cond[[b]][[t]][[7]]
            # add a new column containing the intermediate weights 
            data <- mutate(data, INTER=t_baselines[[t]])
            data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
        }
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
        # get a random integer such that SPORDER == 1
        # r <- sample(nrow(inp), 1, replace=FALSE)
        # while(inp[r]$SPORDER != 1){
        #     r <- sample(nrow(inp), 1, replace=FALSE)
        # }
        r <- 1
        # print(paste("r: ", r))
        # iterate through the rows
        for (i in seq(nrow(inp))){
            of_new <- of_val
            # print(paste("ofnew1: ", of_new))
            h_flag <- TRUE
            if (inp[r]$SPORDER == 1){
                # time a single household
                start_time_hh <- Sys.time()
                upd_flag <- TRUE
                r_temp <- r
                w_delta <- list()
                w_delta[["WGTP"]] <- u_factor * weights[["WGTP"]][r_temp]
                while(h_flag == TRUE){
                    w_delta[["PWGTP"]] <- u_factor * weights[["PWGTP"]][r_temp]
                    # do the change
                    for (b in seq(n_blocks)){
                        # skip if no tables in block
                        if (n_tables[[b]] == 0) {next}
                        # iterate through tables
                        for (t in seq(n_tables[[b]])){
                            # get id of either person or hh table
                            id <- ids[[b]][[t]][r_temp]
                            # skip if id == 0
                            if (id == 0) {next}
                            # get the target and baseline
                            target_val <- targets[[b]][[t]][id]
                            base_old <- baselines[[b]][[t]][id]

                            if (cond[[b]][["special_cond_var"]] == "none"){
                                base_new <- base_old + w_delta[[cond[[b]][["target_var"]]]]
                            }
                            else {
                                base_new <- base_old + w_delta[[cond[[b]][["target_var"]]]]
                            }

                            if (cond[[b]][["special_cond_var"]] == "none"){
                                of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
                            }
                            else {
                                if (r == r_temp){
                                    of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
                                }
                            }
                        }
                    }
                    # increment r_temp to move through persons in a HH
                    if (r_temp != nrow(inp)){
                        r_temp <- r_temp + 1
                    }
                    else {
                        r_temp <- 1
                    }
                    # break out of the loop if a new HH is selected
                    if (inp[r_temp]$SPORDER == 1){
                        # h_flag <- FALSE
                        break
                    }
                }
                # check if OFVal has improved
                if (of_new < of_val){
                    # update the weights
                    i_flag <- TRUE
                    r_temp <- r
                    while (i_flag == TRUE){
                        weights[["WGTP"]][r_temp] <- weights[["WGTP"]][r_temp] + w_delta[["WGTP"]]
                        w_delta[["PWGTP"]] <- weights[["PWGTP"]][r_temp] * u_factor
                        weights[["PWGTP"]][r_temp] <- weights[["PWGTP"]][r_temp] + w_delta[["PWGTP"]]
                        
                        for (b in seq(n_blocks)){
                            for (t in seq(n_tables[[b]])){
                                id <- ids[[b]][[t]][r_temp]
                                
                                if (id > 0){
                                    if ((cond[[b]][["target_var"]] == "WGTP") & (upd_flag == TRUE)){
                                        baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta[["WGTP"]]
                                        upd_flag <- FALSE
                                    }
                                    else if (cond[[b]][["target_var"]] == "PWGTP"){
                                        baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta[["PWGTP"]]
                                    }
                                    
                                }
                            }
                        }

                        # increment r_temp to move through persons in a HH
                        if (r_temp != nrow(inp)){
                            r_temp <- r_temp + 1
                        }
                        else {
                            r_temp <- 1
                        }
                        # break out of the loop if a new HH is selected
                        if (inp[r_temp]$SPORDER == 1){
                            i_flag <- FALSE
                        }
                    } 
                    of_val <- of_new                   
                }
                else {
                    of_new <- of_val
                    h_flag <- TRUE
                    r_temp <- r
                    while(h_flag == TRUE){
                        w_delta[["PWGTP"]] <- u_factor * weights[["PWGTP"]][r_temp]
                        # do the change
                        for (b in seq(n_blocks)){
                            # skip if no tables in block
                            if (n_tables[[b]] == 0) {next}
                            # iterate through tables
                            for (t in seq(n_tables[[b]])){
                                # get id of either person or hh table
                                id <- ids[[b]][[t]][r_temp]
                                # skip if id == 0
                                if (id == 0) {next}
                                # get the target and baseline
                                target_val <- targets[[b]][[t]][id]
                                base_old <- baselines[[b]][[t]][id]

                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    base_new <- base_old - w_delta[[cond[[b]][["target_var"]]]]
                                }
                                else {
                                    base_new <- base_old - w_delta[[cond[[b]][["target_var"]]]]
                                }

                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
                                }
                                else {
                                    if (r == r_temp){
                                        of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
                                    }
                                }
                            }
                        }
                        # increment r_temp to move through persons in a HH
                        if (r_temp != nrow(inp)){
                            r_temp <- r_temp + 1
                        }
                        else {
                            r_temp <- 1
                        }
                        # break out of the loop if a new HH is selected
                        if (inp[r_temp]$SPORDER == 1){
                            h_flag <- FALSE
                        }
                    }
                    if (of_new < of_val){
                        # update the weights
                        i_flag <- TRUE
                        r_temp <- r
                        while (i_flag == TRUE){
                            weights[["WGTP"]][r_temp] <- weights[["WGTP"]][r_temp] - w_delta[["WGTP"]]
                            w_delta[["PWGTP"]] <- weights[["PWGTP"]][r_temp] * u_factor
                            weights[["PWGTP"]][r_temp] <- weights[["PWGTP"]][r_temp] - w_delta[["PWGTP"]]
                        
                            for (b in seq(n_blocks)){
                                for (t in seq(n_tables[[b]])){
                                    id <- ids[[b]][[t]][r_temp]

                                    if (id > 0){
                                        if ((cond[[b]][["target_var"]] == "WGTP") & (upd_flag == TRUE)){
                                            baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_delta[["WGTP"]]
                                            upd_flag <- FALSE
                                        }
                                        else if (cond[[b]][["target_var"]] == "PWGTP"){
                                            baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_delta[["PWGTP"]]
                                        }
                                    
                                    }
                                }
                            }

                            # increment r_temp to move through persons in a HH
                            if (r_temp != nrow(inp)){
                                r_temp <- r_temp + 1
                            }
                            else {
                                r_temp <- 1
                            }   
                            # break out of the loop if a new HH is selected
                            if (inp[r_temp]$SPORDER == 1){
                                i_flag <- FALSE
                            }
                        } 
                        of_val <- of_new                   
                    }
                }
            }
            # increment r while wrapping it around 
            if (r >= nrow(inp)){
                r <- 1
            }
            else {
                r <- r + 1
            }
        }
        # write the data to file
        if (wflag) {
            for (b in seq(n_blocks)){
                for (t in seq(n_tables[[b]])){
                    table <- cond[[b]][[t]]
                    data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
                    data <- mutate(data, INTER=baselines[[b]][[t]])
                    data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
                }
            }
        }
        print(paste("OFVal: ", of_val))
        new_val <- of_val
        
        # check for the early exit condition
        if (abs(new_val - prev_val) == 0) {
            print("Early Exit")
            print(paste("Number of Iterations: ", iter))
            break
        }
        prev_val <- new_val
        end_time <- Sys.time()
        time_taken <- end_time - start_time
        print(paste("Iteration Time: ", time_taken))
    }
    # return the updated weights
    weights <- data.table(
                PWGTP = weights[["PWGTP"]],
                WGTP = weights[["WGTP"]]
                )
    weights <- mutate(weights, SERIALNO=inp$SERIALNO)
    weights <- mutate(weights, SPORDER=inp$SPORDER)
    return(weights)
}