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
    # get the initial geogs as a numeric vector
    geogs <- as.numeric(inp$PLACE)
    temp_geogs <- geogs
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
        # get an integer ordering such that main person of the HH is selected
        r_int <- which(inp$SPORDER == 1)
        # iterate through the rows
        for (r in r_int){
            # store the objective function value
            of_new <- of_val
            # get the list of people in the HH
            child <- cond[["children"]][[r]]
            # get the WGTP
            w_hh <- weights[["WGTP"]][r]
            # iterate through each block
            for (b in seq(n_blocks)){
                # skip block if there are no tables
                if (n_tables[[b]] == 0) {next}
                # iterate through each table
                for (t in seq(n_tables[[b]])){
                    c_flag <- TRUE
                    # iterate through each member of the HH
                    for (c in child){
                        id <- ids[[b]][[t]][c]
                        # skip if it doesn't belong to any of the cells
                        if (id == 0) {next}
                        # get the PWGTP
                        w_p <- weights[["PWGTP"]][c]
                        # get the destination id
                        if (temp_geogs[c] == 1){
                            id_dest <- id + 1
                        }
                        else if(temp_geogs[c] == 2){
                            id_dest <- id - 1
                        }
                        # change the id
                        ids[[b]][[t]][c] <- id_dest
                        # modify the baselines
                        if (cond[[b]][["special_cond_var"]] == "none"){
                            # of_new <- of_new - (abs(target_val_s - base_old_s)) + (abs(target_val_s - base_new_p_s)) - (abs(target_val_d - base_old_d)) + (abs(target_val_d - base_new_p_d))
                            # of_new <- of_new - abs(target_val_s - base_old_s) - abs(target_val_d - base_old_d) + abs(target_val_s - base_new_p_s) + abs(target_val_d - base_new_p_d)
                            baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_p
                            baselines[[b]][[t]][id_dest] <- baselines[[b]][[t]][id_dest] + w_p
                        }
                        else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                            # of_new <- of_new - (abs(target_val_s - base_old_s)) + (abs(target_val_s - base_new_h_s)) - (abs(target_val_d - base_old_d)) + (abs(target_val_d - base_new_h_d))
                            # of_new <- of_new - abs(target_val_s - base_old_s) - abs(target_val_d - base_old_d) + abs(target_val_s - base_new_h_s) + abs(target_val_d - base_new_h_d)
                            baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_hh
                            baselines[[b]][[t]][id_dest] <- baselines[[b]][[t]][id_dest] + w_hh                            
                            c_flag <- FALSE
                        }
                    }
                }
            }
            of_new <- calc_objective(targets, baselines)
            # check if OFVal deteriorates
            if (of_new > of_val){
                # revert the baselines
                for (b in seq(n_blocks)){
                # skip block if there are no tables
                if (n_tables[[b]] == 0) {next}
                # iterate through each table
                    for (t in seq(n_tables[[b]])){
                        # iterate through each member of the HH
                        c_flag <- TRUE
                        for (c in child){
                            id <- ids[[b]][[t]][c]
                            # skip if it doesn't belong to any of the cells
                            if (id == 0) {next}
                            # get the PWGTP
                            w_p <- weights[["PWGTP"]][c]
                            # get the destination id and modify the geogs
                            if (temp_geogs[c] == 1){
                                id_dest <- id - 1
                            }
                            else if (temp_geogs[c] == 2){
                                id_dest <- id + 1
                            }
                            # revert the id
                            ids[[b]][[t]][c] <- id_dest
                            # revert the baselines
                            # print(paste("Before change | base_s: ", baselines[[b]][[t]][id], " |base_d: ", baselines[[b]][[t]][id_dest]))
                            if (cond[[b]][["special_cond_var"]] == "none"){
                                baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_p
                                baselines[[b]][[t]][id_dest] <- baselines[[b]][[t]][id_dest] + w_p
                            }
                            else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag){
                                baselines[[b]][[t]][id] <- baselines[[b]][[t]][id] - w_hh
                                baselines[[b]][[t]][id_dest] <- baselines[[b]][[t]][id_dest] + w_hh
                                c_flag <- FALSE
                            }
                        }
                    }
                }
            }
            else {
                # change the geogs
                for (c in child){
                    if (temp_geogs[c] == 1){
                        temp_geogs[c] <- 2
                    }
                    else if (temp_geogs[c] == 2){
                        temp_geogs[c] <- 1
                    }
                }
            }
        }
        # update the OFVal (workaround since the original ofval calculation is off for r=206)
        of_val <- calc_objective(targets, baselines)
        print(paste("OFVal after initial placement: ", of_val))
        # begin exchanging households
        # create a new column that stores the temporary geogs
        inp$TPLACE <- temp_geogs
        g1_list <- which(inp$TPLACE == 1 & inp$SPORDER == 1)
        g2_list <- which(inp$TPLACE == 2 & inp$SPORDER == 1)
        for (h1 in g1_list){
            # print(paste("h1: ", h1))
            ex_flag <- FALSE
            for (h2 in g2_list){
                child1 <- cond[["children"]][[h1]]
                child2 <- cond[["children"]][[h2]]
                w_hh1 <- weights[["WGTP"]][h1]
                w_hh2 <- weights[["WGTP"]][h2]
                # iterate through each block
                for (b in seq(n_blocks)){
                    # skip block if there are no tables
                    if (n_tables[[b]] == 0) {next}
                    # iterate through each table
                    for (t in seq(n_tables[[b]])){
                        c_flag1 <- TRUE
                        c_flag2 <- TRUE 
                        # iterate through each member of the first HH
                        for (c1 in child1){
                            id1 <- ids[[b]][[t]][c1]
                            # skip if it doesn't belong to any cell
                            if (id1 == 0) {next}
                            # get the PWGTP
                            w_p1 <- weights[["PWGTP"]][c1]
                            # get the destination id
                            if (temp_geogs[c1] == 1){
                                id_dest1 <- id1 + 1
                            }
                            else if (temp_geogs[c1] == 2){
                                id_dest1 <- id1 - 1
                            }
                            # change the id
                            ids[[b]][[t]][c1] <- id_dest1
                            # change the baseline values
                            if (cond[[b]][["special_cond_var"]] == "none"){
                                baselines[[b]][[t]][id1] <- baselines[[b]][[t]][id1] - w_p1
                                baselines[[b]][[t]][id_dest1] <- baselines[[b]][[t]][id_dest1] + w_p1
                            }
                            else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag1){
                                baselines[[b]][[t]][id1] <- baselines[[b]][[t]][id1] - w_hh1
                                baselines[[b]][[t]][id_dest1] <- baselines[[b]][[t]][id_dest1] + w_hh1
                                c_flag1 <- FALSE
                            }
                        }
                        for (c2 in child2){
                            id2 <- ids[[b]][[t]][c2]
                            # skip if it doesn't belong to any cell
                            if (id2 == 0) {next}
                            # get the PWGTP
                            w_p2 <- weights[["PWGTP"]][c2]
                            # get the destination id
                            if (temp_geogs[c2] == 1){
                                id_dest2 <- id2 + 1
                            }
                            else if (temp_geogs[c2] == 2){
                                id_dest2 <- id2 - 1
                            }
                            # change the id
                            ids[[b]][[t]][c2] <- id_dest2
                            # change the baseline values
                            if (cond[[b]][["special_cond_var"]] == "none"){
                                baselines[[b]][[t]][id2] <- baselines[[b]][[t]][id2] - w_p2
                                baselines[[b]][[t]][id_dest2] <- baselines[[b]][[t]][id_dest2] + w_p2
                            }
                            else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag2){
                                baselines[[b]][[t]][id2] <- baselines[[b]][[t]][id2] - w_hh2
                                baselines[[b]][[t]][id_dest2] <- baselines[[b]][[t]][id_dest2] + w_hh2
                                c_flag2 <- FALSE 
                            }
                        }
                    }
                }
                # get the new OFVal
                of_new <- calc_objective(targets, baselines)
                # if the objective is worse, revert the changes
                if (of_new > of_val){
                    # revert the exchange
                    # revert the baselines
                    # iterate through each block
                    # print(paste("before: ", of_val))
                    for (b in seq(n_blocks)){
                        # skip block if there are no tables
                        if (n_tables[[b]] == 0) {next}
                        # iterate through each table
                        for (t in seq(n_tables[[b]])){
                            c_flag1 <- TRUE
                            c_flag2 <- TRUE 
                            # iterate through each member of the first HH
                            for (c1 in child1){
                                id1 <- ids[[b]][[t]][c1]
                                # skip if it doesn't belong to any cell
                                if (id1 == 0) {next}
                                # get the PWGTP
                                w_p1 <- weights[["PWGTP"]][c1]
                                # get the destination id
                                if (temp_geogs[c1] == 1){
                                    id_dest1 <- id1 - 1
                                }
                                else if (temp_geogs[c1] == 2){
                                    id_dest1 <- id1 + 1
                                }
                                # revert the id
                                ids[[b]][[t]][c1] <- id_dest1
                                # change the baseline values
                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    baselines[[b]][[t]][id1] <- baselines[[b]][[t]][id1] - w_p1
                                    baselines[[b]][[t]][id_dest1] <- baselines[[b]][[t]][id_dest1] + w_p1
                                }
                                else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag1){
                                    baselines[[b]][[t]][id1] <- baselines[[b]][[t]][id1] - w_hh1
                                    baselines[[b]][[t]][id_dest1] <- baselines[[b]][[t]][id_dest1] + w_hh1
                                    c_flag1 <- FALSE
                                }
                            }
                            for (c2 in child2){
                                id2 <- ids[[b]][[t]][c2]
                                # skip if it doesn't belong to any cell
                                if (id2 == 0) {next}
                                # get the PWGTP
                                w_p2 <- weights[["PWGTP"]][c2]
                                # get the destination id
                                if (temp_geogs[c2] == 1){
                                    id_dest2 <- id2 - 1
                                }
                                else if (temp_geogs[c2] == 2){
                                    id_dest2 <- id2 + 1
                                }
                                # revert the id
                                ids[[b]][[t]][c2] <- id_dest2
                                # revert the baselines
                                if (cond[[b]][["special_cond_var"]] == "none"){
                                    baselines[[b]][[t]][id2] <- baselines[[b]][[t]][id2] - w_p2
                                    baselines[[b]][[t]][id_dest2] <- baselines[[b]][[t]][id_dest2] + w_p2
                                }
                                else if (cond[[b]][["special_cond_var"]] == "SPORDER" & c_flag2){
                                    baselines[[b]][[t]][id2] <- baselines[[b]][[t]][id2] - w_hh2
                                    baselines[[b]][[t]][id_dest2] <- baselines[[b]][[t]][id_dest2] + w_hh2
                                    c_flag2 <- FALSE 
                                }
                            }
                        }
                    }
                }
                else {
                    # print(paste("OFVAl: ", of_val, " |OFNEW: ", of_new))
                    g2_list <- g2_list[-h2]
                    # change the geogs
                    for (c1 in child1){
                        if (temp_geogs[c1] == 1){
                            temp_geogs[c1] <- 2
                        }
                        else if (temp_geogs[c1] == 2){
                            temp_geogs[c1] <- 1
                        }
                    }
                    for (c2 in child2){
                        if (temp_geogs[c2] == 1){
                            temp_geogs[c2] <- 2
                        }
                        else if (temp_geogs[c2] == 2){
                            temp_geogs[c2] <- 1
                        }
                    }
                    ex_flag <- TRUE
                }
                of_val <- calc_objective(targets, baselines)
                if (ex_flag) {break}
            }
        }
        # write the data to file
        if (wflag){
            for (b in seq(n_blocks)){
                # skip if no tables
                if (n_tables[[b]] == 0) {next}
                for (t in seq(n_tables[[b]])){
                    table <- cond[[b]][[t]]
                    data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
                    data <- mutate(data, INTER=baselines[[b]][[t]])
                    data <- data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
                }
            }
        }
        # save the new objective value
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
        print(paste("Iter No.: ", iter, " | Time: ", time_taken, " | OFVal: ", of_val))
    }
    # return the assigned geogs
    areas <- data.table(
                PLACE = temp_geogs
            )
    areas <- mutate(areas, SERIALNO=inp$SERIALNO)
    areas <- mutate(areas, SPORDER=inp$SPORDER)
    return(areas)
}