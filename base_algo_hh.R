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
    for (i in seq(num_iter)){
        # store the previous OFVal
        prev_val <- of_val
        start_time <- Sys.time()
        # get a random integer such that SPORDER == 1
        # r <- sample(nrow(inp), 1, replace=FALSE)
        # while(inp[r]$SPORDER != 1){
        #     r <- sample(nrow(inp), 1, replace=FALSE)
        # }
        r <- 14
        # print(paste("r: ", r))
        # iterate through the rows
        for (i in seq(nrow(inp))){
            of_new <- of_val
            # print(paste("ofnew1: ", of_new))
            h_flag <- TRUE
            if (inp[r]$SPORDER == 1){
                r_temp <- r
                w_delta <- list()
                w_delta[["WGTP"]] <- u_factor * weights[["WGTP"]][r_temp]
                while(h_flag == TRUE){
                    w_delta[["PWGTP"]] <- u_factor * weights[["PWGTP"]][r_temp]
                    # print(paste("r: ", r, " wdelh: ", w_delta[["WGTP"]], " wdelp: ", w_delta[["PWGTP"]]))
                    # print(paste("r: ", r, " r_temp: ", r_temp))
                    # do the change
                    for (b in seq(n_blocks)){
                        # skip if no tables in block
                        if (n_tables[[b]] == 0) {next}
                        # iterate through tables
                        for (t in seq(n_tables[[b]])){
                            # get id of either person or hh table
                            id <- ids[[b]][[t]][r_temp]
                            # print(paste("id: ", id))
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
                            # print(cond[[b]][["target_var"]])
                            # print(paste("target: ", target_val, " base: ", base_old))
                            # print(paste("base_old1: ", base_old))
                            # print(paste("base_new1: ", base_new))
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
                print(of_new)
                # check if OFVal has improved
                if (of_new < of_val){
                    # update the weights
                    i_flag <- TRUE
                    r_temp <- r
                    
                    while (i_flag == TRUE){
                        print(paste("r_temp: ", r_temp))
                        print(weights[["WGTP"]])
                        print(weights[["WGTP"]][r_temp])
                        print(paste("r: ", r))
                        print(paste("r_temp: ", r_temp))
                        print(w_delta[["WGTP"]])
                        weights[["WGTP"]][r_temp] <- weights[["WGTP"]][r_temp] + w_delta[["WGTP"]]
                        w_delta[["PWGTP"]] <- weights[["PWGTP"]][r_temp] * u_factor
                        print(paste("w_del_p: ", w_delta[["PWGTP"]]))
                        weights[["PWGTP"]][r_temp] <- weights[["PWGTP"]][r_temp] + w_delta[["PWGTP"]]
                        print(weights[["WGTP"]][r_temp])
                        print(weights[["PWGTP"]][r_temp])
                        print(weights[["WGTP"]])
                        print(weights[["PWGTP"]])

                        for (b in seq(n_blocks)){
                            for (t in seq(n_tables[[b]])){
                                id <- ids[[b]][[t]][r_temp]
                                print(paste("id: ", id))
                                if (id > 0){
                                    print(baselines[[b]][[1]])
                                    quit()
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
                    quit()
                }

            }
            quit()

            # increment r while wrapping it around 
            if (r >= nrow(inp)){
                r <- 1
            }
            else {
                r <- r + 1
            }
            # for (t in seq(n_tables)){
            #     id <- ids[[t]][r]
            #     if (id == 0){next}
            #     if (target_var == "WGTP"){
            #         if (inp[[r]]$SPORDER == 1){
            #             # iterate through the household
            #             target_val <- targets[[t]][id]
            #             base_old <- baselines[[t]][id]
            #             base_new <- base_old + w_delta
            #             r_temp <- r + 1
            #             # update the objective function
            #             while(inp[[r_temp]]$SPORDER != 1){
            #                 target_val <- targets[[t]][id]
            #                 base_old <- baselines[[t]][id]
            #                 base_new <- base_old + w_delta
            #                 r_temp <- r + 1
            #                 # update the objective function

            #             }

            #         }
            #         else{next}
            #     }
            #     else{
            #     target_val <- targets[[t]][id]
            #     base_old <- baselines[[t]][id]
            #     base_new <- base_old + w_delta
            #     # print(paste("base_old1: ", base_old))
            #     # print(paste("base_new1: ", base_new))
            #     }
            #     of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
            #     # print(paste("ofnew2: ", of_new))
            # }
            # # check whether the new OFVal is better than the old OFVal when increasing the 
            # # weight by the update factor
            # if (of_new < of_val){
            #     # update the weights
            #     weights[r] <- weights[r] + w_delta
            #     for (t in seq(n_tables)){
            #         id <- ids[[t]][r]
            #         if (id > 0) {
            #             baselines[[t]][id]  <- baselines[[t]][id] + w_delta
            #         }
            #     }
            #     of_val <- of_new
            # }
            # # if not, decrease the weight by the update factor and try again
            # else {
            #     of_new <- of_val
            #     for (t in seq(n_tables)){
            #         id <- ids[[t]][r]
            #         if (id == 0){next}
            #         target_val <- targets[[t]][id]
            #         base_old <- baselines[[t]][id]
            #         base_new <- base_old - w_delta
            #         # print(paste("base_old2: ", base_old))
            #         # print(paste("base_new2: ", base_new))
            #         of_new <- of_new - (target_val - base_old) ^ 2 + (target_val - base_new) ^ 2
            #         # print(paste("ofnew3: ", of_new))
            #     }
            #     # check whether the OFVal has improved
            #     if (of_new < of_val){
            #         weights[r] <- weights[r] - w_delta
            #         for (t in seq(n_tables)){
            #             id <- ids[[t]][r]
            #             if (id > 0){
            #                 baselines[[t]][id]  <- baselines[[t]][id] - w_delta
            #             }
            #         }
            #         of_val <- of_new
            #     }
            # }
        }
        quit(status=1)
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
        if (abs(new_val - prev_val) == 0) {
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
    weights <- data.table(weights)
    weights <- mutate(weights, SERIALNO=inp$SERIALNO)
    weights <- mutate(weights, SPORDER=inp$SPORDER)
    return(weights)
}