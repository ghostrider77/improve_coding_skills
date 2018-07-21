#!/usr/bin/env Rscript
Operations <- list("+"=.Primitive("+"), "-"=.Primitive("-"), "*"=.Primitive("*"))

ReadInputData <- function(conn) {
    line <- unlist(strsplit(readLines(conn, n=1), split = ""))
    len <- length(line)
    digits <- as.integer(line[seq(from = 1, to = len, by = 2)])
    operations <- line[seq(from = 2, to = len, by = 2)]
    list(digits=digits, operations=operations)
}

CalcMinMax <- function(ix, jy, operations, minimumOfSubexpressions, maximumOfSubexpressions) {
    subexpressionMin <- Inf
    subexpressionMax <- -Inf
    for (k in ix:(jy-1)) {
        op <- Operations[[operations[k]]]
        a <- op(maximumOfSubexpressions[ix, k], maximumOfSubexpressions[k+1, jy])
        b <- op(maximumOfSubexpressions[ix, k], minimumOfSubexpressions[k+1, jy])
        c <- op(minimumOfSubexpressions[ix, k], maximumOfSubexpressions[k+1, jy])
        d <- op(minimumOfSubexpressions[ix, k], minimumOfSubexpressions[k+1, jy])
        subexpressionMin <- min(subexpressionMin, a, b, c, d)
        subexpressionMax <- max(subexpressionMax, a, b, c, d)
    }
    list(maximum=subexpressionMax, minimum=subexpressionMin)
}

MaximizeArithmeticExpression <- function(digits, operations) {
    n <- length(digits)
    minimumOfSubexpressions <- matrix(data = 0, nrow = n, ncol = n)
    maximumOfSubexpressions <- matrix(data = 0, nrow = n, ncol = n)
    diag(minimumOfSubexpressions) <- digits
    diag(maximumOfSubexpressions) <- digits
    for (s in 1:(n-1)) {
        for (ix in 1:(n-s)) {
            jy <- ix + s
            extremeValues <- CalcMinMax(ix, jy, operations, minimumOfSubexpressions, maximumOfSubexpressions)
            minimumOfSubexpressions[ix, jy] <- extremeValues$minimum
            maximumOfSubexpressions[ix, jy] <- extremeValues$maximum
        }
    }
    maximumOfSubexpressions[1, n]
}

Main <- function() {
    conn <- file("stdin", "r")
    inputData <- ReadInputData(conn)
    close(conn)
    result <- MaximizeArithmeticExpression(inputData$digits, inputData$operations)
    cat(format(result, digits = 18), fill = TRUE)
}

if (!interactive()) {
    Main()
}
