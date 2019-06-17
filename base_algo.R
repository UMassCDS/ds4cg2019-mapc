# base randomized descent algorithm
# import libraries
library(dplyr)
library(data.table)
# import custom functions
source("objective.R")

# randomized descent algoritm
random_descent <- function(inp, cond, num_iter, u_factor, wflag) {
    n_tables <- length(cond) - 2
    inp_file <- cond[["file_name"]]
    target_var <- cond[["target_var"]]
    weights <- as.numeric(inp[[target_var]])

    ids <- list()
    baselines <- list()
    targets <- list()

    for (t in seq(n_tables)){
        table <- cond[[t]]
        data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
        baselines[[t]] <- as.numeric(data[["BASELINE"]])
        targets[[t]] <- as.numeric(data[["TARGET"]])
        ids[[t]] <- cond[[t]][[7]]

        data <- mutate(data, INTER=baselines[[t]])
        data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
        
        # print(head(data))
    }

    of_val <- calc_objective(targets, baselines)
    print(paste("init ofval: ", of_val))
    for (i in seq(num_iter)){
        start_time <- Sys.time()
        r_int <- sample(nrow(inp), nrow(inp), replace=FALSE)
        # r_int <- sample(nrow(inp), 1)
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

            if (of_new < of_val){
                weights[r] <- weights[r] + w_delta
                for (t in seq(n_tables)){
                    id <- ids[[t]][r]
                    if (id > 0) {
                        baselines[[t]][id]  <- baselines[[t]][id] + w_delta
                    }
                }
                of_val <- of_new
            }
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
        if (wflag) {
            for (t in seq(n_tables)){
                table <- cond[[t]]
                data <- data.table::fread(file=paste(table[[1]], ".csv", sep=""))
                data <- mutate(data, INTER=baselines[[t]])
                data.table::fwrite(data, file=paste(table[[1]], ".csv", sep=""))
            }
        }
        print(paste("OFVal: ", of_val))
        end_time <- Sys.time()
        time_taken <- end_time - start_time
        print(paste("Iteration Time: ", time_taken))
    }
    return(weights)
}