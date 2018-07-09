#!/usr/bin/env Rscript
HasMajorityElem <- function(lst, n) {
    counts <- as.vector(table(lst))
    if (any(counts > n %/% 2)) 1 else 0
}

Main <- function() {
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 2)
    close(conn)
    n <- as.integer(lines[1])
    lst <- as.integer(unlist(strsplit(lines[2], split = " ")))
    result <- HasMajorityElem(lst, n)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
