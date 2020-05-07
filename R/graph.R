#' pg_graph
#'
#' Construct function network graph for a package
#'
#' @param pkg_dir Directory containing the package
#' @param plot If `TRUE`, plot the network using \pkg{visNetwork} which opens an
#' interactive browswer pane.
#' @return A `list` of `nodes` and `edges` describing connections between the
#' various exported and non-exported functions of a package.
#' @export
pg_graph <- function (pkg_dir, plot = TRUE) {
    pkgmap <- pkgapi::map_package (pkg_dir)

    # suppress no visible binding notes:
    from <- to <- NULL
    edges <- pkgmap$calls %>%
        dplyr::group_by (from, to) %>%
        dplyr::summarise (n = length (from)) %>%
        dplyr::ungroup ()
    edges$from <- paste0 (pkgmap$name, "::", edges$from)
    nodes <- unique (c (edges$from, edges$to))
    export <- gsub (paste0 (pkgmap$name, "::"), "", nodes) %in% pkgmap$exports
    nodes <- tibble::tibble (name = nodes,
                             export = export)

    # reduce to only package-internal calls:
    nodes <- nodes [grep (paste0 ("^", pkgmap$name, "::"), nodes$name), ]
    edges <- edges [grep (paste0 ("^", pkgmap$name, "::"), edges$to), ]

    nodes$name <- gsub (paste0 ("^", pkgmap$name, "::"), "", nodes$name)
    edges$from <- gsub (paste0 ("^", pkgmap$name, "::"), "", edges$from)
    edges$to <- gsub (paste0 ("^", pkgmap$name, "::"), "", edges$to)

    nodes <- nodes [nodes$name != "", ]

    # attach file start and end points to nodes
    index <- match (nodes$name, pkgmap$defs$name)
    nodes$file <- pkgmap$defs$file [index]
    nodes$line1 <- pkgmap$defs$line1 [index]
    nodes$line2 <- pkgmap$defs$line2 [index]

    cl <- igraph::graph_from_data_frame (edges, directed = FALSE) %>%
        igraph::clusters ()
    nodes$group <- cl$membership [match (nodes$name, names (cl$membership))] %>%
        as.integer ()
    index <- which (is.na (nodes$group))
    nodes$group [index] <- max (nodes$group, na.rm = TRUE) + seq (index)

    nodes$centrality <- node_centrality (nodes, edges)

    if (plot) {
        edges$width <- 10 * edges$n
        nodes$value <- nodes$centrality
        nodes$id <- nodes$label <- nodes$name
        vn <- visNetwork::visNetwork (nodes, edges,
                                      main = paste0 (pkgmap$name, " network"))

        print (vn)
        edges$width <- NULL
        nodes$value <- nodes$label <- nodes$id <- NULL
    }

    docs <- doc_lines (pkg_dir, nodes)
    nodes$doc_lines <- docs$lines [match (docs$name, nodes$name)]

    res <- list (nodes = nodes, edges = edges)
    attr (res, "pkg_name") <- pkg_name (pkg_dir)

    return (res)
}

node_centrality <- function (nodes, edges)
{
    ig <- igraph::graph_from_data_frame (edges, vertices = nodes)
    ig <- igraph::set_edge_attr (ig, "weight",
                                 value = igraph::edge.attributes (ig)$n)
    btw <- igraph::betweenness (ig)
    btw [match (names (btw), nodes$name)]
}

# count documentation lines preceding all fn defintions
doc_lines_one_file <- function (pkg_dir, nodes, filename) {
    nds <- nodes [nodes$file == filename, ]
    nds <- nds [order (nds$line1), ]

    x <- readLines (file.path (pkg_dir, filename))
    x <- split (x, findInterval (seq (length (x)), nds$line2 + 1)) [seq (nrow (nds))]

    #index <- which (!nodes$export)
    #x <- x [index]

    nlines <- vapply (x, function (i) {
                          i <- i [which (!grepl ("nocov st", i))]
                          ftemp <- file.path (tempdir (), "junk.R")
                          writeLines (i, ftemp)
                          p <- getParseData (parse (file = ftemp))
                          return (which (p$token != "COMMENT") [1] - 1)
                                 }, numeric (1))

    data.frame (name = nds$name,
                lines = nlines,
                stringsAsFactors = FALSE)
}

doc_lines <- function (pkg_dir, nodes) {
    files <- unique (nodes$file)
    res <- lapply (files, function (i) doc_lines_one_file (pkg_dir, nodes, i))
    do.call (rbind, res)
}
