# function to compute objective 
library(data.table)
library(dplyr)

calc_objective <- function(base) {

    # print(base$TARGET)
    # print(target$TARGET)

    base_vec <- base$BASELINE
    target_vec <- base$TARGET 
    objective <- sum(sqrt((base_vec - target_vec) ** 2))

    return(objective)

}

b <- data.table::fread(file="FirstTable.csv")
obj_value <- calc_objective(b)
print(paste("OFValue: ", obj_value))
