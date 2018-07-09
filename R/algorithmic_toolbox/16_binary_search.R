#!/usr/bin/env Rscript
GetListAndSize <- function(line) {
    lst <- as.integer(unlist(strsplit(line, split = " ")))
    list(n=lst[1], lst=lst[-1])
}

FindElem <- function(elem, lst, n) {
    BinarySearch <- function(left, right) {
        if (left > right) return(-1)
        middleIx <- (left + right) %/% 2
        middleElem <- lst[middleIx]
        if (middleElem == elem) {
            middleIx
        } else if (middleElem < elem) {
            BinarySearch(middleIx + 1, right)
        } else BinarySearch(left, middleIx - 1)
    }
    BinarySearch(1, n)
}

FindElemsInList <- function(lst, n, queries) {
    indices <- sapply(queries, FindElem, lst, n)
    sapply(indices, function(ind) if (ind == -1) ind else ind -1)
}

Main <- function() {
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 2)
    close(conn)
    lst <- GetListAndSize(lines[1])
    queries <- GetListAndSize(lines[2])
    result <- FindElemsInList(lst$lst, lst$n, queries$lst)
    cat(result, fill = TRUE)
}

if (!interactive()) {
    Main()
}
