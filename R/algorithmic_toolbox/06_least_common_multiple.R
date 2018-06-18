#!/usr/bin/env Rscript
CheckPrerequisites <- function(pkgs) {
    for (pkg in pkgs) {
        if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
            install.packages(pkg, dependencies = TRUE)
            suppressMessages(suppressWarnings(require(pkg, quietly = TRUE)))
        }
    }
}

CalcGCD <- function(a, b) {
    while (b > 0) {
        rem <- a %% b
        a <- b
        b <- rem
    }
    a
}

CalcLCM <- function(a, b) {
    gcd <- CalcGCD(a, b)
    as.integer64(a / gcd) * b
}

Main <- function() {
    pkgs <- c("bit64")
    CheckPrerequisites(pkgs)
    conn <- file("stdin", "r")
    line <- readLines(conn, n = 1)
    close(conn)
    lst <- as.integer(unlist(strsplit(line, split = " ")))
    result <- CalcLCM(lst[1], lst[2])
    cat(format(result, digits = 20), fill = TRUE)
}

if (!interactive()) {
    Main()
}
