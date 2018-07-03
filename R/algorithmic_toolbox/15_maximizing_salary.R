#!/usr/bin/env Rscript
Number <- function(n) {
    wrapper <- list(asString=n)
    wrapper
}

"[.number" <- function(ns, ix) {
    class(ns) <- "list"
    n <- ns[ix]
    class(n) <- "number"
    n
}

"==.number" <- function(w1, w2) {
    n1 <- w1[[1]]$asString
    n2 <- w2[[1]]$asString
    paste(n1, n2, sep = "") == paste(n2, n1, sep = "")
}

">.number" <- function(w1, w2) {
    n1 <- w1[[1]]$asString
    n2 <- w2[[1]]$asString
    paste(n1, n2, sep = "") > paste(n2, n1, sep = "")
}

AssembleLargestNumberFromPieces <- function(numberStrings) {
    numbers <- lapply(numberStrings, Number)
    class(numbers) <- "number"
    unname(unlist(sort(numbers, decreasing = TRUE)))
}

Main <- function() {
    conn <- file("stdin", "r")
    numberStrings <- as.list(as.integer(unlist(strsplit(readLines(conn, n = 2)[2], split = " "))))
    close(conn)
    result <- AssembleLargestNumberFromPieces(numberStrings)
    cat(paste0(result, collapse = ""), fill = TRUE)
}

if (!interactive()) {
    Main()
}
