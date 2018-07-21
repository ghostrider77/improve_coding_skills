#!/usr/bin/env Rscript
ReadInputData <- function(conn) {
    params <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    names(params) <- c("capacity", "nrWeights")
    weights <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    list(weights=weights, nrWeights=params["nrWeights"], capacity=params["capacity"])
}

SolveKnapsackProblem <- function(weights, nrWeights, capacity) {
    knapsack <- matrix(data = -1, nrow = capacity, ncol = nrWeights)
    solve <- function(currentCapacity, n) {
        if (currentCapacity == 0 || n == 0) return(0)
        if (knapsack[currentCapacity, n] != -1) return(knapsack[currentCapacity, n])
        weight <- weights[n]
        optimalWeight <- if (currentCapacity < weight) solve(currentCapacity, n - 1) else {
            max(solve(currentCapacity - weight, n - 1) + weight, solve(currentCapacity, n - 1))
        }
        knapsack[currentCapacity, n] <- optimalWeight
        optimalWeight
    }
    solve(capacity, nrWeights)
}

Main <- function() {
    conn <- file("stdin", "r")
    inputData <- ReadInputData(conn)
    close(conn)
    result <- SolveKnapsackProblem(inputData$weights, inputData$nrWeights, inputData$capacity)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
