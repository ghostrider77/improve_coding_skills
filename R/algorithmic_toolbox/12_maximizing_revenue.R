#!/usr/bin/env Rscript
CheckPrerequisites <- function(pkgs) {
    for (pkg in pkgs) {
        if (!suppressMessages(require(pkg, character.only = TRUE, quietly = TRUE))) {
            install.packages(pkg, dependencies = TRUE)
            suppressMessages(suppressWarnings(require(pkg, quietly = TRUE)))
        }
    }
}

CalcMaximalRevenue <- function(profitPerClick, averageClickPerDay) {
    sum(as.integer64(sort(profitPerClick)) * sort(averageClickPerDay))
}

Main <- function() {
    pkgs <- c("bit64")
    CheckPrerequisites(pkgs)
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 3)
    close(conn)
    profitPerClick <- as.integer(unlist(strsplit(lines[2], split = " ")))
    averageClickPerDay <- as.integer(unlist(strsplit(lines[3], split = " ")))
    result <- CalcMaximalRevenue(profitPerClick, averageClickPerDay)
    cat(format(result, digits = 20), fill = TRUE)
}

if (!interactive()) {
    Main()
}
