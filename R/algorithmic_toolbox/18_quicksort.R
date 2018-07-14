#!/usr/bin/env Rscript
QuickSort <- function(lst, n) {
    set.seed(2112)
    ThreeWayPartitioning <- function(pivot, start, end) {
        ix <- start
        while (ix <= end) {
            elem <- lst[ix]
            if (elem < pivot) {
                if (ix != start) lst[c(ix, start)] <<- lst[c(start, ix)]
                ix <- ix + 1
                start <- start + 1
            } else if (elem > pivot) {
                lst[c(ix, end)] <<- lst[c(end, ix)]
                end <- end - 1
            } else ix <- ix + 1
        }
        list(left=start, right=end)
    }

    intervalStack <- list(list(left=1, right=n))
    size <- 1
    while (size > 0) {
        interval <- intervalStack[[size]]
        intervalStack[[size]] <- NULL
        size <- size - 1
        if (interval$left < interval$right) {
            randomIx <- sample(seq(interval$left, interval$right), 1)
            pivot <- lst[randomIx]
            middleInterval <- ThreeWayPartitioning(pivot, interval$left, interval$right)
            intervalStack[[size+1]] <- list(left=interval$left, right=middleInterval$left-1)
            intervalStack[[size+2]] <- list(left=middleInterval$right+1, right=interval$right)
            size <- size + 2
        }
    }
    lst
}

Main <- function() {
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 2)
    close(conn)
    n <- as.integer(lines[1])
    lst <- as.integer(unlist(strsplit(lines[2], split = " ")))
    result <- QuickSort(lst, n)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
