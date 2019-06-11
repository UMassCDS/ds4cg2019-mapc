# function to compute objective 
library(data.table)
library(dplyr)

calc_objective <- function(base, target) {

    # print(base$TARGET)
    # print(target$TARGET)

    base_vec <- base$TARGET
    target_vec <- target$TARGET 
    objective <- sum(sqrt((base_vec - target_vec) ** 2))

    return(objective)

}

b <- data.table::fread(file="FirstTable.csv")
t <- data.table::fread(file="FirstTarget.csv")
obj_value <- calc_objective(b, t)
print(paste("OFValue: ", obj_value))
