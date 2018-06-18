#!/usr/bin/env Rscript
CalcGCD <- function(a, b) {
    while (b > 0) {
        rem <- a %% b
        a <- b
        b <- rem
    }
    a
}

Main <- function() {
    conn <- file("stdin", "r")
    line <- readLines(conn, n = 1)
    close(conn)
    lst <- as.integer(unlist(strsplit(line, split = " ")))
    result <- CalcGCD(lst[1], lst[2])
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
