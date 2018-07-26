#!/usr/bin/env Rscript
bruteForceSize <- 3

ReadInputData <- function(conn) {
    nrPoints <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    points <- matrix(0, nrow = nrPoints, ncol = 2)
    colnames(points) <- c("x", "y")
    for (ix in 1:nrPoints) {
        points[ix, ] <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    }
    list(nrPoints=nrPoints, points=points)
}

Distance <- function(p, q)  {
    sqrt(sum((p - q)^2))
}

GetSmallestPairwiseDistances <- function(points, nrPoints, minDist, compareWith) {
    if (nrPoints <= 1) return(minDist)
    for (ix in 1:(nrPoints-1)) {
        p <- points[ix,]
        nrNextPoints <- min(ix + 1 + compareWith, nrPoints)
        for (jy in (ix+1):nrNextPoints) {
            q <- points[jy,]
            d <- Distance(p, q)
            if (d < minDist) minDist = d
        }
    }
    minDist
}

FindPointsInStripe <- function(first, second, m, delta) {
    rbind(first[abs(first[,"x"] - m) <= delta,], second[abs(second[,"x"] - m) <= delta,])
}

CalcMinimumDistanceInStripe <- function(first, second, medianX, delta) {
    stripe <- FindPointsInStripe(first, second, medianX, delta)
    sortedStripe <- stripe[order(stripe[,"y"]),, drop=FALSE]
    GetSmallestPairwiseDistances(sortedStripe, nrow(sortedStripe), delta, 7)
}

FindClosestPoints <- function(sortedPoints, nrPoints) {
    if (nrPoints <= bruteForceSize) {
        return(GetSmallestPairwiseDistances(sortedPoints, nrPoints, Inf, bruteForceSize - 1))
    }
    middle <- (nrPoints + 1) %/% 2
    medianX <- sortedPoints[middle, "x"]
    first <- sortedPoints[1:middle,]
    second <- sortedPoints[(middle+1):nrPoints,]
    delta1 <- FindClosestPoints(first, middle)
    delta2 <- FindClosestPoints(second, nrPoints-middle)
    delta <- min(delta1, delta2)
    if (abs(delta) <= 1e-14) 0.0 else CalcMinimumDistanceInStripe(first, second, medianX, delta)
}

FindClosestPairOfPoints <- function(points, nrPoints) {
    sortedPoints <- points[order(points[,"x"]),]
    FindClosestPoints(sortedPoints, nrPoints)
}

Main <- function() {
    conn <- file("stdin", "r")
    inputData <- ReadInputData(conn)
    close(conn)
    result <- FindClosestPairOfPoints(inputData$points, inputData$nrPoints)
    cat(format(result, digits = 16), fill = TRUE)
}

if (!interactive()) {
    Main()
}
