# function to compute objective, modified for hh weights
library(data.table)
library(dplyr)

calc_objective <- function(target, baseline) {

    objective <- 0
    for (i in seq_along(target)){
        for (j in seq_along(target[[i]])){
            objective <- objective + sum((baseline[[i]][[j]] - target[[i]][[j]]) ** 2)
        }
    }
    return(objective)
}
