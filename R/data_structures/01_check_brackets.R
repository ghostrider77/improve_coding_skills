#!/usr/bin/env Rscript
openingBrackets <- c("(", "[", "{")
closingBrackets <- c(")", "]", "}")

doBracketsMatch <- function(openingBracket, closingBracket) {
    ((openingBracket == "[" && closingBracket == "]") ||
     (openingBracket == "(" && closingBracket == ")") ||
     (openingBracket == "{" && closingBracket == "}"))
}

retrieveFailedOpeningIndexFromStack <- function(stack, size) {
    if (size == 0) NULL
    else {
        openedBracket <- stack[[size]]
        openedBracket$position
    }
}

findIndexOfNonMatchingBracket <- function(string) {
    stack <- list()
    size <- 0
    for (ix in 1:length(string)) {
        letter <- string[ix]
        if (letter %in% openingBrackets) {
            size <- size + 1
            stack[[size]] <- list(bracket=letter, position=ix)
        } else if (letter %in% closingBrackets) {
            if (size == 0) return(ix)
            openedBracket <- stack[[size]]
            stack[[size]] <- NULL
            size <- size - 1
            if (!doBracketsMatch(openedBracket$bracket, letter)) return(ix)
        }
    }
    retrieveFailedOpeningIndexFromStack(stack, size)
}

Main <- function() {
    conn <- file("stdin", "r")
    string <- readLines(conn, n = 1)
    close(conn)
    charList <- strsplit(string, "")[[1]]
    failedIndex <- findIndexOfNonMatchingBracket(charList)
    cat(if (is.null(failedIndex)) "Success" else as.character(failedIndex), fill = TRUE)
}

if (!interactive()) {
    Main()
}
