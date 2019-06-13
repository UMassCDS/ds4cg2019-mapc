# function to compute objective 
library(data.table)
library(dplyr)

calc_objective <- function(target, baseline) {

    objective <- 0
    for (i in seq_along(target)){
        objective <- objective + sum((baseline[[i]] - target[[i]]) ** 2)
    }

    return(objective)
}
