#!/usr/bin/env Rscript
CheckPrerequisites <- function(pkgs) {
    for (pkg in pkgs) {
        if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
            install.packages(pkg, dependencies = TRUE)
            suppressMessages(suppressWarnings(require(pkg, quietly = TRUE)))
        }
    }
}

CalcMaximumPairwiseProduct <- function(lst) {
    twoLargestElems <- c(-Inf, -Inf)
    names(twoLargestElems) <- c("largest", "second_largest")
    for (elem in lst) {
        if (elem > twoLargestElems["largest"]) {
            twoLargestElems["second_largest"] <- twoLargestElems["largest"]
            twoLargestElems["largest"] <- elem
        } else if (elem > twoLargestElems["second_largest"]) {
            twoLargestElems["second_largest"] <- elem
        }
    }
    as.integer64(twoLargestElems["largest"]) * twoLargestElems["second_largest"]
}

Main <- function() {
    pkgs <- c("bit64")
    CheckPrerequisites(pkgs)
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 2)
    close(conn)
    lst <- as.integer(unlist(strsplit(lines[2], " ")))
    result <- CalcMaximumPairwiseProduct(lst)
    cat(paste(result), fill = TRUE)
}

if (!interactive()) {
    Main()
}
