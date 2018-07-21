#!/usr/bin/env Rscript
ReadInputData <- function(conn) {
    lines <- readLines(conn, n=6)
    extractSeq <- function(k) {
        list(length=as.integer(lines[2*k-1]), sequence=as.integer(unlist(strsplit(lines[2*k], split = " "))))
    }
    lapply(1:3, extractSeq)
}

LongestCommonSubsequence <- function(s1, s2, s3){
    longestPath <- array(data = 0, dim = c(s1$length+1, s2$length+1, s3$length+1))
    for (i in 1:s1$length) {
        for (j in 1:s2$length) {
            for (k in 1:s3$length) {
                if (s1$sequence[i] == s2$sequence[j] && s1$sequence[i] == s3$sequence[k]) {
                    longestPath[i+1, j+1, k+1] <- longestPath[i, j, k] + 1
                } else {
                    longestPath[i+1, j+1, k+1] <- max(longestPath[i, j+1, k+1],
                                                      longestPath[i+1, j, k+1],
                                                      longestPath[i+1, j+1, k])
                }
            }
        }
    }
    longestPath[s1$length+1, s2$length+1, s3$length+1]
}

Main <- function() {
    conn <- file("stdin", "r")
    inputData <- ReadInputData(conn)
    close(conn)
    result <- LongestCommonSubsequence(inputData[[1]], inputData[[2]], inputData[[3]])
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
