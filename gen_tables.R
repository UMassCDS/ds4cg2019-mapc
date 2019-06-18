# this file is unused 
# generate the target and baseline matrices for the values
# load libraries
library(dplyr)
library(data.table)
# generate the targets and baseline values
gen_tables <- function(new_tables, f_tables){
    targets <- list()
    baselines <- list()
    for (i in seq_along(f_tables)){
        data.table::fwrite(new_tables[[i]], file=f_tables[[i]])
        targets[[i]] <- new_tables[[i]]$TARGET 
        baselines[[i]] <- new_tables[[i]]$INTER
    }
    temp <- list()
    temp[[1]] <- targets
    temp[[2]] <- baselines
    return(temp)
}
