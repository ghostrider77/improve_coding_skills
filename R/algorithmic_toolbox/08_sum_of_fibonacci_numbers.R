#!/usr/bin/env Rscript

modulus <- 10

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

CalcLastDigitOfTheSumOfFibonacciNumbers <- function(n) {
    p <- CalcPisanoPeriod(modulus)
    (CalcFibonacciModulo((n + 2) %% p, modulus) - 1) %% modulus
}

Main <- function() {
    conn <- file("stdin", "r")
    n <- as.integer(readLines(conn, n=1))
    close(conn)
    digit <- CalcLastDigitOfTheSumOfFibonacciNumbers(n)
    cat(digit, fill = TRUE)
}

if (!interactive()) {
    Main()
}
