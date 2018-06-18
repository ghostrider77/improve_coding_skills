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

CalcLastDigitOfPartialSum <- function(m, n) {
    p <- CalcPisanoPeriod(modulus)
    lastDigitOfPrefixSum <- (CalcFibonacciModulo((m + 1) %% p, modulus) - 1) %% modulus
    lastDigitOfFullSum <- (CalcFibonacciModulo((n + 2) %% p, modulus) - 1) %% modulus
    (lastDigitOfFullSum - lastDigitOfPrefixSum) %% modulus
}

Main <- function() {
    conn <- file("stdin", "r")
    line <- readLines(conn, n = 1)
    lst <- as.integer(unlist(strsplit(line, split = " ")))
    close(conn)
    digit <- CalcLastDigitOfPartialSum(lst[1], lst[2])
    cat(digit, fill = TRUE)
}

if (!interactive()) {
    Main()
}
