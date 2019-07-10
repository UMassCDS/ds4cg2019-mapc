# base randomized descent algorithm with functionality for changing HH weights as well as Person weights
# optimized
# import libraries
library(dplyr)
library(data.table)
# import custom functions
source("objective.R")

# randomized descent algoritm
# INPUTS:-
# inp: data table of input (POP2016.csv)
# cond: data table of savefile.RData
# num_iter: number of iterations the algorithm should run
# u_factor: factor by which the weights are updated
# wflag: flag whether to write data to file
# OUTPUTS: set of updated weights 
random_descent_hh <- function(inp, cond, num_iter, u_factor, wflag) {
    # get the number of tables and blocks
    n_blocks <- length(cond) - 2
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
        r_int <- sample(nrow(inp), nrow(inp), replace=FALSE)
        # print(paste("r: ", r))
        # iterate through the rows
        for (r in r_int){
            if (inp[r]$SPORDER == 1){
                of_new <- of_val
                child <- cond[["children"]][[r]]
                # print(paste("r: ", r))
                # print(child)
                w_delta_h <- u_factor * weights[["WGTP"]][r]
                # print(paste("DelWGTP: ", w_delta_h))
                # for (c in child) {
                #     w_delta_p <- u_factor * weights[["PWGTP"]][c]
                #     print(paste("DelPWGTP: ", w_delta_p))
                #     for (b in seq(n_blocks)){
                #         if (n_tables[[b]] == 0) {next}
                #         c_flag <- TRUE
                #         for (t in seq(n_tables[[b]])){
                #             id <- ids[[b]][[t]][c]
                #             if (id == 0) {next}
                #             target_val <- targets[[b]][[t]][id]
                #             base_old <- baselines[[b]][[t]][id]
                #             base_new_p <- base_old + w_delta_p
                #             base_new_h <- base_old + w_delta_h
                #             if (cond[[b]][["special_cond_var"]] == "none"){
                #                 of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new_p) ^ 2
                #             }
                #             else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                #                 of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new_h) ^ 2
                #                 c_flag <- FALSE
                #             }
                #         }
                #     }
                # }

                for (b in seq(n_blocks)){
                    if (n_tables[[b]] == 0) {next}
                    for (t in seq(n_tables[[b]])){
                        c_flag <- TRUE
                        for (c in child){
                            id <- ids[[b]][[t]][c]
                            if (id == 0) {next}
                            w_delta_p <- u_factor * weights[["PWGTP"]][c]
                            # print(paste("DelPWGTP: ", w_delta_p))
                            target_val <- targets[[b]][[t]][id]
                            base_old <- baselines[[b]][[t]][id]
                            base_new_p <- base_old + w_delta_p
                            base_new_h <- base_old + w_delta_h
                            if (cond[[b]][["special_cond_var"]] == "none"){
                                of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new_p) ^ 2
                            }
                            else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                                of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new_h) ^ 2
                                c_flag <- FALSE
                            }
                        }
                    }
                }
                of_new <- of_new
                # check if OFVal improves
                if (of_new < of_val){
                #     for (c in child){
                #         w_delta_p <- u_factor * weights[["PWGTP"]][c]
                #         weights[["WGTP"]][c] <- weights[["WGTP"]][c] + w_delta_h
                #         weights[["PWGTP"]][c] <- weights[["PWGTP"]][c] + w_delta_p
                #         for (b in seq(n_blocks)){
                #             c_flag <- TRUE
                #             if (n_tables[[b]] == 0) {next}
                #             for (t in seq(n_tables[[b]])){
                #                 id <- ids[[b]][[t]][c]
                #                 if (id == 0){next}
                #                 if (cond[[b]][["special_cond_var"]] == "none"){
                #                     baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta_p
                #                 }
                #                 else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                #                     baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta_h
                #                     c_flag <- FALSE
                #                 }
                #             }
                #         }
                #     }

                    
                    # modify the baseline vectors
                    for (b in seq(n_blocks)){
                        if (n_tables[[b]] == 0) {next}
                        for (t in seq(n_tables[[b]])){
                            c_flag <- TRUE
                            for (c in child){
                                w_delta_p <- u_factor * weights[["PWGTP"]][c]
                                id <- ids[[b]][[t]][c]
                                if (id == 0) {next}
                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta_p
                                }
                                else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                                    baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta_h
                                    c_flag <- FALSE
                                }
                            }
                            
                        }
                    }

                    # modify the weight vectors
                    for (c in child){
                        w_delta_p <- u_factor * weights[["PWGTP"]][c]
                        weights[["WGTP"]][c] <- weights[["WGTP"]][c] + w_delta_h
                        weights[["PWGTP"]][c] <- weights[["PWGTP"]][c] + w_delta_p
                    }

                    of_val <- of_new
                }
                else {
                    of_new <- of_val
                    for (b in seq(n_blocks)){
                        if(n_tables[[b]] == 0) {next}
                        for (t in seq(n_tables[[b]])){
                            c_flag <- TRUE
                            for (c in child){
                                id <- ids[[b]][[t]][c]
                                if (id == 0) {next}
                                w_delta_p <- u_factor * weights[["PWGTP"]][c]
                                # print(paste("DelPWGTP: ", w_delta_p))
                                target_val <- targets[[b]][[t]][id]
                                base_old <- baselines[[b]][[t]][id]
                                base_new_p <- base_old - w_delta_p
                                base_new_h <- base_old - w_delta_h
                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new_p) ^ 2
                                }
                                else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                                    of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new_h) ^ 2
                                    c_flag <- FALSE
                                }
                            }
                        }
                    }
                    # check if OFVal improves
                    if (of_new < of_val){
                        # modify the baseline vectors
                        for (b in seq(n_blocks)){
                            if (n_tables[[b]] == 0) {next}
                            for (t in seq(n_tables[[b]])){
                                c_flag <- TRUE
                                for (c in child){
                                    w_delta_p <- u_factor * weights[["PWGTP"]][c]
                                    id <- ids[[b]][[t]][c]
                                    if (id == 0) {next}
                                    if (cond[[b]][["special_cond_var"]] == "none"){
                                        baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_delta_p
                                    }
                                    else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                                        baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_delta_h
                                        c_flag <- FALSE
                                    }
                                }
                            
                            }
                        }

                        # modify the weight vectors
                        for (c in child){
                            w_delta_p <- u_factor * weights[["PWGTP"]][c]
                            weights[["WGTP"]][c] <- weights[["WGTP"]][c] - w_delta_h
                            weights[["PWGTP"]][c] <- weights[["PWGTP"]][c] - w_delta_p
                        }

                        of_val <- of_new

                    }
                }
            }
        }
        if (wflag){
            for (b in seq(n_blocks)){
                if (n_tables[[b]] == 0) {next}
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
        print(paste("Iter No.: ", iter, " | Time: ", time_taken))
    }
    return(weights)
}
