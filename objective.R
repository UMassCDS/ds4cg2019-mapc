# function to compute objective 
library(data.table)
library(dplyr)

calc_objective <- function(table) {

    # print(base$TARGET)
    # print(target$TARGET)

    base_vec <- table$INTER
    target_vec <- table$TARGET 
    objective <- sqrt(sum((base_vec - target_vec) ** 2))

    return(objective)

}
