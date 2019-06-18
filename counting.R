# this file is unused
# load libraries
library(dplyr)
library(readr)
library(stringr)
library(data.table)

# import custom functions
source("conversion.R")

# set the data directory
# directory = 'path/to/directory'
# setwd(directory)
# list.files()

# function to generate counts from data table
# inp: Input CSV object
# base: Base Matrix CSV object
# conditions: .RData savefile created by read_config.R
gen_counts <- function(inp, tables, conditions) {
    outs <- list()
    # read the files
    # source <- data.table::fread(file=inp)   # input file containing the data
    source <- inp   # input data table
    cond <- conditions  # .RData savefile
    # iterate through the different tables
    for (t in seq_along(tables)){
        # baseline <- data.table::fread(file=base[[t]], fill=TRUE) # input file containing the baseline matrices
        baseline <- tables[[t]]
        # generate a list of variable names, number of variable conditions
        # start_time <- Sys.time()
        var_names <- c(cond[[t]][[3]])  # list of variable names
        dim_vec <- cond[[t]][[2]]  # list of variable dimensions
        hvec <- get_hvec(dim_vec)
        num_dims <- length(dim_vec)
        num_conds <- prod(dim_vec)
        # print(paste("num_conds:", num_conds))
        # generate the aggregate weights
        new_weights <- NULL
        # iterate through conditions
        for (i in 1:num_conds) {
            cd <- index2coord(i, dim_vec, hvec)  # coordinates for every index
            # print(paste("cd:", cd))
            temp_s <- ""    # init an empty string to generate the 'subset' condition
            # iterate through the variables
            for (j in seq_along(var_names)){
                # add a '&' between conditions
                if (j > 1){
                    temp_s <- paste(temp_s, "&")
                }
                # generate the text condition
                temp <- cond[[t]][[5]][[j]][[cd[[j]]]] %>%
                    str_replace_all("x", "source[[\"x\"]]") %>%
                    str_replace_all("x", var_names[[j]])
                # concatenate the generated string with the existing string
                temp_s <- paste(temp_s, temp)
            }
            # wrap the string with the original data table name
            temp_s <- paste("source[", temp_s, "]")
            # print(temp_s)
            # evaluate the string to generate the subset of the data table
            temp_df <- eval(parse(text=temp_s))
            # head(temp_df)
            # generate a string to examine the target variable type
            w_string <- paste("sum(temp_df$", cond["target_var"],")")
            # print(w_string)
            # evaluate the string to sum the weights
            weight_sum <- eval(parse(text=w_string))
            # add evaluated sum to the existing weight vector
            new_weights <- c(new_weights, weight_sum)
        }
        # end_time <- Sys.time()
        # time <- end_time - start_time
        # print(paste("Time: ", time))
        # generate the baseline matrix
        baseline <- mutate(baseline, INTER=new_weights)
        # write the baseline matrix to a csv file
        # data.table::fwrite(baseline, file=base[[t]])
        outs[[t]] <- baseline
    }
    return(outs)
}
