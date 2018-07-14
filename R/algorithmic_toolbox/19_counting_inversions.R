#!/usr/bin/env Rscript
MergeSortedArrays <- function(first, second, size1, size2, inversions) {
    merged <- numeric(size1+size2)
    ix <- 1
    jy <- 1
    for (k in 1:(size1+size2)) {
        if (ix > size1 && jy <= size2) {
            merged[k] <- second[jy]
            jy <- jy + 1
        } else if (ix <= size1 && jy > size2) {
            merged[k] <- first[ix]
            ix <- ix + 1
        } else {
            x <- first[ix]
            y <- second[jy]
            if (x <= y) {
                merged[k] <- x
                ix <- ix + 1
            } else {
                merged[k] <- y
                jy <- jy + 1
                inversions <- inversions + (size1 - ix + 1)
            }
        }
    }
    list(lst=merged, inversions=inversions)
}

CountInversions <- function(lst, size) {
    if (size <= 1) return(list(lst=lst, inversions=0))
    middle <- size %/% 2
    first <- lst[1:middle]
    second <- lst[(middle+1):size]
    size1 <- middle
    size2 <- size - middle
    sortedFirst <- CountInversions(first, size1)
    sortedSecond <- CountInversions(second, size2)
    inversionsInHalfArrays <- sortedFirst$inversions + sortedSecond$inversions
    MergeSortedArrays(sortedFirst$lst, sortedSecond$lst, size1, size2, inversionsInHalfArrays)
}

Main <- function() {
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 2)
    close(conn)
    n <- as.integer(lines[1])
    lst <- as.integer(unlist(strsplit(lines[2], split = " ")))
    result <- CountInversions(lst, n)
    cat(result$inversions, fill = TRUE)
}

if (!interactive()) {
    Main()
}
