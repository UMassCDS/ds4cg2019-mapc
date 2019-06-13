# baseline randomized descent algorithm
# import libraries
library(dplyr)
# import custom functions
source("objective.R")
# randomized descent algoritm
random_descent <- function(inp, tables, conditions) {
    # source <- inp
    # cond <- conditions
    # print(paste("rows:", nrow(inp)))
    # print(paste("cols:", ncol(inp)))
    r_int <- sample(1:nrow(inp), 1)
    # print(r_int)
    # increase the weight by 1% 
    print(inp[1, ]$PWGTP)
    # print(inp[[r_int]]$PWGTP)
}