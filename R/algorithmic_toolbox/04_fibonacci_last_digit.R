
modulus <- 10

FibonacciLastDigit <- function(n) {
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

Main <- function() {
    conn <- file("stdin", "r")
    n <- as.integer(readLines(conn, n=1))
    close(conn)
    digit <- FibonacciLastDigit(n)
    cat(digit, fill = TRUE)
}

if (!interactive()) {
    Main()
}
