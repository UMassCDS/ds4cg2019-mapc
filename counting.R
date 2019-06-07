# load libraries
library(dplyr)
library(readr)
library(stringr)
library(data.table)
library(itertools)

# import custom functions
source("read_config.R")

# set the data directory
# directory = 'path/to/directory'
# setwd(directory)
# list.files()

# read the csv file for inputs and targets
# data <- data.table::fread(file='./data/trial.csv')  # needs to be generalized
# baseline <- data.table::fread(file='./FirstTable.csv', fill=TRUE)  # needs to be generalized
# sfile <- read_rds("savefile.RData")  # needs to be generalized

# generate the column names for the data
coln <- colnames(data)
# print(coln) 

# function to generate counts from POP2016
gen_counts <- function(inp, base) {
    # read the files
    source <- data.table::fread(file=inp)   # input file containing the data
    baseline <- data.table::fread(file=base, fill=TRUE) # input file containing the baseline matrices
    cond <- read_rds("savefile.RData")  # .RData file

    # generate a list of variable names, number of variable conditions
    var_names <- c(cond[[1]][[3]])
    dim_vec <- cond[[1]][[2]]
    hvec <- get_hvec(dim_vec)
    num_dims <- length(dim_vec)
    num_conds <- prod(dim_vec)
    
    # generate the aggregate weights
    new_weights <- NULL
    for (i in 1:num_conds) {
        cd <- index2coord(i)
        temp_s <- ""
        for (j in seq_along(var_names)){
            if (j > 1){
                temp_s <- paste(temp_s, "&")
            }
            temp <- cond[[1]][[5]][[j]][[cd[[j]]]] %>%
                str_replace_all("x", "source[[\"x\"]]") %>%
                str_replace_all("x", var_names[[j]])
            temp_s <- paste(temp_s, temp)
        }
        temp_s <- paste("source[", temp_s, "]")
        temp_df <- eval(parse(text=temp_s))
        # print(temp_df)
        weight_sum <- sum(temp_df$PWGTP)
        new_weights <- c(new_weights, weight_sum)
    }
    
    baseline <- mutate(baseline, TARGET=new_weights)
    print(baseline)
    data.table::fwrite(baseline, file=base)
}
