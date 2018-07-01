#!/usr/bin/env Rscript
ReadSegments <- function(conn) {
    nrSegments <- as.integer(readLines(conn, n = 1))
    segments <- matrix(0, nrow = nrSegments, ncol = 2)
    colnames(segments) <- c("left", "right")
    for (ix in 1:nrSegments) {
        segments[ix, ] <- as.integer(unlist(strsplit(readLines(conn, n = 1), split = " ")))
    }
    segments
}

CalcMinimumNumberOfPointsCoveringSegments <- function(segments) {
    sortedSegments <- segments[order(segments[, "right"]),]
    points <- vector(mode = "integer", length = 0)
    while (nrow(sortedSegments) > 0) {
        b <- sortedSegments[1, "right"]
        points <- c(points, b)
        sortedSegments <- sortedSegments[sortedSegments[, "left"] > b,,drop = FALSE]
    }
    points
}

Main <- function() {
    conn <- file("stdin", "r")
    segments <- ReadSegments(conn)
    close(conn)
    result <- CalcMinimumNumberOfPointsCoveringSegments(segments)
    cat(length(result), fill = TRUE)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
