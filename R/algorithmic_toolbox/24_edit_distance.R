#!/usr/bin/env Rscript
LevenshteinDistance <- function(s1, s2){
    n = length(s1)
    m = length(s2)
    editDistance <- matrix(data = 0, nrow = n+1, ncol = m+1)
    editDistance[, 1] <- seq(from = 0, to = n)
    editDistance[1,] <- seq(from = 0, to = m)
    for (ix in 1:n) {
        for (jy in 1:m) {
            deletion <- editDistance[ix, jy+1] + 1
            insertion <- editDistance[ix+1, jy] + 1
            match <- editDistance[ix, jy] + (if (s1[ix] == s2[jy]) 0 else 1)
            editDistance[ix+1, jy+1] <- min(deletion, insertion, match)
        }
    }
    editDistance[n+1, m+1]
}

Main <- function() {
    conn <- file("stdin", "r")
    s1 <- unlist(strsplit(readLines(conn, n=1), split = ""))
    s2 <- unlist(strsplit(readLines(conn, n=1), split = ""))
    close(conn)
    result <- LevenshteinDistance(s1, s2)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
