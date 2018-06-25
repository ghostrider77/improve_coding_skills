#!/usr/bin/env Rscript

coins <- c(10, 5, 1)

CalcMinimumNumberOfChanges <- function(amount) {
    numberOfChanges <- 0
    for (coin in coins) {
        numberOfChanges <- numberOfChanges + amount %/% coin
        amount <- amount %% coin
    }
    numberOfChanges
}

Main <- function() {
    conn <- file("stdin", "r")
    amount <- as.integer(readLines(conn, n=1))
    close(conn)
    result <- CalcMinimumNumberOfChanges(amount)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
