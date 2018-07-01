#!/usr/bin/env Rscript
DecomposeToDistinctElems <- function(n) {
    smallestSummand <- 1
    summands <- vector(mode = "integer", length = as.integer(sqrt(2*n)))
    while (n > 0) {
        elem <- if (n > 2 * smallestSummand) smallestSummand else n
        summands[smallestSummand] <- elem
        n <- n - elem
        smallestSummand <- smallestSummand + 1
    }
    summands[summands > 0]
}

Main <- function() {
    conn <- file("stdin", "r")
    n <- as.integer(readLines(conn, n = 1))
    close(conn)
    result <- DecomposeToDistinctElems(n)
    cat(length(result), fill = TRUE)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
