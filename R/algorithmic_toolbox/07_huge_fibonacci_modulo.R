#!/usr/bin/env Rscript
CalcPisanoPeriod <- function(modulus) {
    p <- 1
    a <- 1
    b <- 1
    while (!(a == 0 && b == 1)) {
        c <- (a + b) %% modulus
        a <- b
        b <- c
        p <- p + 1
    }
    p
}

CalcFibonacciModulo <- function(n, modulus) {
    k <- 1
    a <- 0
    b <- 1
    while (k <= n) {
        c <- (a + b) %% modulus
        a <- b
        b <- c
        k <- k + 1
    }
    a
}

CalcHugeFibonacciModulo <- function(n, modulus) {
    p <- CalcPisanoPeriod(modulus)
    CalcFibonacciModulo(n %% p, modulus)
}

Main <- function() {
    conn <- file("stdin", "r")
    line <- readLines(conn, n = 1)
    lst <- as.integer(unlist(strsplit(line, split = " ")))
    close(conn)
    f <- CalcHugeFibonacciModulo(lst[1], lst[2])
    cat(f, fill = TRUE)
}

if (!interactive()) {
    Main()
}
