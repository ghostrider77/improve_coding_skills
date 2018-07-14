#!/usr/bin/env Rscript
ReadInputData <- function(conn) {
    nrSegments <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))[1]
    segments <- matrix(0, nrow = nrSegments, ncol = 2)
    colnames(segments) <- c("left", "right")
    for (ix in 1:nrSegments) {
        segments[ix, ] <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    }
    points <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    list(nrSegments=nrSegments, segments=segments, points=points)
}

getNumberOfSuitableEndpoints <- function(endPoints, size, point) {
    BinarySearch <- function(a, b) {
        if (a == b) return(a)
        mid <- (a + b) %/% 2
        if (endPoints[mid] <= point) BinarySearch(mid + 1, b) else BinarySearch(a, mid)
    }
    if (endPoints[size] <= point) size else BinarySearch(1, size) - 1
}

CalcIntersectionSize <- function(point, sortedLeft, sortedNegatedRight, nrSegments) {
    intervalsFromLeft <- getNumberOfSuitableEndpoints(sortedLeft, nrSegments, point)
    intervalsFromRight <- getNumberOfSuitableEndpoints(sortedNegatedRight, nrSegments, -point)
    intervalsFromLeft + intervalsFromRight - nrSegments
}

NumberOfSegmentsContainingPoints <- function(endPoints, nrSegments, points) {
    sortedLeft <- sort(endPoints[,"left"])
    sortedNegatedRight <- sort(-endPoints[,"right"])
    sapply(points, CalcIntersectionSize, sortedLeft, sortedNegatedRight, nrSegments)
}

Main <- function() {
    conn <- file("stdin", "r")
    inputData <- ReadInputData(conn)
    close(conn)
    result <- NumberOfSegmentsContainingPoints(inputData$segments, inputData$nrSegments, inputData$points)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
