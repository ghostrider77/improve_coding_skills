#!/usr/bin/env Rscript
nominators <- c(2, 3)

findPreviousMinimum <- function(operations, k) {
    m <- k - 1
    previousMin <- operations[m]
    minArg <- m
    for (nom in nominators) {
        if (k %% nom == 0) {
            m = k %/% nom
            numberOfOps <- operations[m]
            if (numberOfOps < previousMin) {
                previousMin <- numberOfOps
                minArg <- m
            }
        }
    }
    list(value=previousMin, index=minArg)
}

backTrackCalculation <- function(backTrack, n) {
    path = n
    k <- n
    while (k > 1) {
        k <- backTrack[k]
        path <- c(path, k)
    }
    rev(path)
}

RunCalculator <- function(n) {
    minOperations <- vector(mode = "numeric", length = n)
    backTrack <- vector(mode = "numeric", length = n)
    if (n > 1) {
        for (k in 2:n) {
            previousMin <- findPreviousMinimum(minOperations, k)
            minOperations[k] <- previousMin$value + 1
            backTrack[k] <- previousMin$index
        }
    }
    backTrackCalculation(backTrack, n)
}

Main <- function() {
    conn <- file("stdin", "r")
    n <- as.integer(readLines(conn, n=1))
    close(conn)
    result <- RunCalculator(n)
    cat(length(result) - 1, fill = TRUE)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
