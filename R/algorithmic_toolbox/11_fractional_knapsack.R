#!/usr/bin/env Rscript

ReadInputData <- function(conn) {
    params <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    names(params) <- c("nrItems", "capacity")
    items <- matrix(0, nrow = params["nrItems"], ncol = 2)
    colnames(items) <- c("value", "weight")
    for (ix in 1:params["nrItems"]) {
        items[ix, ] <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    }
    list(capacity=params["capacity"], items=items)
}

SolveFractionalKnapsack <- function(items, capacity) {
    valuePerUnitWeight <- apply(items, 1, function(row) row["value"] / row["weight"])
    orderedIndices <- order(valuePerUnitWeight, decreasing = TRUE)
    totalValue <- 0
    for (ix in orderedIndices) {
        if (capacity == 0) return(totalValue)
        row <- items[ix, ]
        amount <- min(row["weight"], capacity)
        totalValue <- totalValue + amount * (row["value"] / row["weight"])
        capacity <- capacity - amount
    }
    totalValue
}

Main <- function() {
    conn <- file("stdin", "r")
    inputData <- ReadInputData(conn)
    close(conn)
    result <- SolveFractionalKnapsack(inputData$items, inputData$capacity)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
