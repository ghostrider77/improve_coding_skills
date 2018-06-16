
AddTwoNumbers <- function(a, b) {
    a + b
}

Main <- function() {
    conn <- file("stdin", "r")
    a <- as.integer(readLines(conn, n=1))
    b <- as.integer(readLines(conn, n=1))
    close(conn)
    result <- AddTwoNumbers(a, b)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
