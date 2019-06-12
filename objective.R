# function to compute objective 
library(data.table)
library(dplyr)

calc_objective <- function(table) {

    num_cols <- length(table)
    base_vec <- table$INTER
    target_vec <- table$TARGET 
    objective <- sqrt(sum((base_vec - target_vec) ** 2))

    return(objective)

}
