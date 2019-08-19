#!/usr/bin/env Rscript
buildTree <- function(nrNodes, parentsOfNodes) {
    nodes <- data.frame(key=0:(nrNodes-1), parentIds=parentsOfNodes)
    parentsToChildren <- split(nodes$key, nodes$parentIds)
    root <- parentsToChildren[["-1"]]
    parentsToChildren[["-1"]] <- NULL
    list(root=root, edges=parentsToChildren)
}

getChildrenOfNodes <- function(edges, newKeys) {
    children <- c()
    for (key in newKeys) {
        children <- c(children, edges[[as.character(key)]])
    }
    children
}

calcTreeDepth <- function(root, edges) {
    newKeys <- c(root)
    depth <- 0
    while (!is.null(newKeys)) {
        depth <- depth + 1
        newKeys <- getChildrenOfNodes(edges, newKeys)
    }
    depth
}

Main <- function() {
    conn <- file("stdin", "r")
    lines <- readLines(conn, n = 2)
    close(conn)
    nrNodes <- as.integer(lines[1])
    parentIds <- as.integer(unlist(strsplit(lines[2], split = " ")))
    tree <- buildTree(nrNodes, parentIds)
    height <- calcTreeDepth(tree$root, tree$edges)
    cat(format(height, scientific = FALSE), fill = TRUE)
}

if (!interactive()) {
    Main()
}
