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
    nodes <- tibble::tibble (id = nodes,
                             label = nodes,
                             export = export)

    # reduce to only package-internal calls:
    nodes <- nodes [grep (paste0 ("^", pkgmap$name, "::"), nodes$id), ]
    edges <- edges [grep (paste0 ("^", pkgmap$name, "::"), edges$to), ]

    nodes$id <- gsub (paste0 ("^", pkgmap$name, "::"), "", nodes$id)
    nodes$label <- gsub (paste0 ("^", pkgmap$name, "::"), "", nodes$label)
    edges$from <- gsub (paste0 ("^", pkgmap$name, "::"), "", edges$from)
    edges$to <- gsub (paste0 ("^", pkgmap$name, "::"), "", edges$to)

    cl <- igraph::graph_from_data_frame (edges) %>%
        igraph::clusters ()
    nodes$group <- "1"
    for (i in 2:cl$no) {
        index <- which (nodes$id %in%
                        names (cl$membership [which (cl$membership == i)]))
        nodes$group [index] <- paste0 (i)
    }

    if (plot) {
        edges$width <- 10 * edges$n
        vn <- visNetwork::visNetwork (nodes, edges,
                                      main = paste0 (pkgmap$name, " network"))

        print (vn)
        edges$width <- NULL
    }

    return (list (nodes = nodes, edges = edges))
}
