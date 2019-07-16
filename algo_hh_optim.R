# base randomized descent algorithm with functionality for changing HH weights as well as Person weights
# optimized
# import libraries
library(dplyr)
library(data.table)
# increase the number of significant digits
options(digits=12)
# library(bit64)
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
        # get a random integer ordering
        r_int <- sample(nrow(inp), nrow(inp), replace=FALSE)
        # r_int <- c(4)
        # iterate through the rows
        for (r in r_int){
            # check if it's the start of a new HH
            if (inp[r]$SPORDER == 1){
                # store the objective function value
                of_new <- of_val
                # get the list of people in the HH
                child <- cond[["children"]][[r]]
                # get the updated WGTP weight differential
                w_delta_h <- u_factor * weights[["WGTP"]][r]
                # iterate through each block
                for (b in seq(n_blocks)){
                    # skip block if there are no tables
                    if (n_tables[[b]] == 0) {next}
                    # iterate through each table
                    for (t in seq(n_tables[[b]])){
                        # iterate through each member of the HH
                        # print("new")
                        # print(paste("CompOFVal Start: ", of_new))
                        for (c in child){
                            id <- ids[[b]][[t]][c]
                            # skip if it doesn't belong to any of the cells
                            if (id == 0) {next}
                            # get the updated PWGTP weight differential
                            w_delta_p <- u_factor * weights[["PWGTP"]][c]
                            # get the baseline and target value
                            target_val <- targets[[b]][[t]][id]
                            base_old <- baselines[[b]][[t]][id]

                            # get the new baseline values for both PWGTP and WGTP
                            base_new_p <- base_old + w_delta_p
                            base_new_h <- base_old + w_delta_h

                            # if (t == 2){
                            #     print(paste("target_val", target_val))
                            #     print(paste("base_old: ", base_old))
                            #     print(paste("base_new_p: ", base_new_p))
                            # }
                            # get the modified objective function value
                            
                            if (cond[[b]][["special_cond_var"]] == "none"){
                                # print("person")
                                # print(paste("WDelP: ", w_delta_p))
                                # print(paste("base_new_p", base_new_p))
                                # print(paste("start: ", of_new))
                                of_new <- of_new - ((target_val - base_old) ^ 2) + ((target_val - base_new_p) ^ 2)
                                # print(paste("end: ", of_new))
                            }
                            else if (cond[[b]][["special_cond_var"]] == "SPORDER"){
                                # print("household")
                                # print(paste("WDelH: ", w_delta_h))
                                # print(paste("base_new_h", base_new_h))
                                # print(paste("start: ", of_new))
                                of_new <- of_new - ((target_val - base_old) ^ 2) + ((target_val - base_new_h) ^ 2)
                                # print(paste("end: ", of_new))
                                break
                            }
                        }
                        # break
                        # print(paste("CompOFValEnd: ", of_new))
                    }
                }
                # check if OFVal improves
                if (of_new < of_val){
                    # print("increase")
                    # modify the baseline vectors
                    for (b in seq(n_blocks)){
                        # skip if no tables
                        if (n_tables[[b]] == 0) {next}
                        for (t in seq(n_tables[[b]])){
                            # iterate though members of the the HH
                            for (c in child){
                                w_delta_p <- u_factor * weights[["PWGTP"]][c]
                                id <- ids[[b]][[t]][c]
                                # skip if it doesn't belong to any of the cells
                                if (id == 0) {next}
                                # modify the baselines accordingly
                                # print(baselines[[b]][[t]][id])
                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta_p
                                }
                                else if (cond[[b]][["special_cond_var"]] == "SPORDER"){
                                    baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] + w_delta_h
                                    break
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
                    # update the objective value
                    of_val <- of_new
                    # print(paste("Updated OFVal: ", of_val))
                }
                # if objective does not improve, go in the other direction
                else {
                    # print("decrease")
                    of_new <- of_val
                    for (b in seq(n_blocks)){
                        # skip if no tables
                        if(n_tables[[b]] == 0) {next}
                        # iterate through the tables
                        for (t in seq(n_tables[[b]])){
                            # iterate through members of the HH
                            for (c in child){
                                id <- ids[[b]][[t]][c]
                                # skip if it doesn't belong to any of the cells
                                if (id == 0) {next}
                                w_delta_p <- u_factor * weights[["PWGTP"]][c]
                                # print(paste("DelPWGTP: ", w_delta_p))
                                target_val <- targets[[b]][[t]][id]
                                base_old <- baselines[[b]][[t]][id]
                                base_new_p <- base_old - w_delta_p
                                base_new_h <- base_old - w_delta_h
                                # modify the objective function value
                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    of_new <- of_new - ((target_val - base_old) ^ 2) + ((target_val - base_new_p) ^ 2)
                                    # print(of_new)
                                }
                                else if (cond[[b]][["special_cond_var"]] == "SPORDER"){
                                    of_new <- of_new - ((target_val - base_old) ^ 2) + ((target_val - base_new_h) ^ 2)
                                    # print(of_new)
                                    break
                                }
                            }
                        }
                    }
                    # check if OFVal improves
                    if (of_new < of_val){
                        # modify the baseline vectors
                        for (b in seq(n_blocks)){
                            # skip if no tables
                            if (n_tables[[b]] == 0) {next}
                            for (t in seq(n_tables[[b]])){
                                for (c in child){
                                    w_delta_p <- u_factor * weights[["PWGTP"]][c]
                                    id <- ids[[b]][[t]][c]
                                    # skip if it doesn't belong to any of the cells
                                    if (id == 0) {next}
                                    # modify the baselines accordingly
                                    if (cond[[b]][["special_cond_var"]] == "none"){
                                        baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_delta_p
                                    }
                                    else if (cond[[b]][["special_cond_var"]] == "SPORDER"){
                                        baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_delta_h
                                        break
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
                        # update the objective value
                        of_val <- of_new
                    }
                }
                # print(paste("final new: ", of_new))
                # print(paste("final val: ", of_val))
                # print(paste("OFVAL: ", calc_objective(targets, baselines)))
                # quit()
            }
        }
        # write the baselines to file
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
        # report the new objective value
        of_temp <- calc_objective(targets, baselines)
        print(paste("OFVal: ", of_val, " | Temp: ", of_temp))
        new_val <- of_val
        # check for the early exit condition
        if (abs(new_val - prev_val) == 0) {
            print("Early Exit")
            print(paste("Number of Iterations: ", iter))
            break
        }
        prev_val <- new_val
        end_time <- Sys.time()
        # report the iteration time
        time_taken <- end_time - start_time
        print(paste("Iter No.: ", iter, " | Time: ", time_taken))
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
