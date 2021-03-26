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
    nodes$group [index] <- max (nodes$group, na.rm = TRUE) +
        seq (length (index))

    edges <- edges [which (!(edges$from == "" | edges$to == "")), ]
    nodes$centrality <- node_centrality (nodes, edges)

    if (plot) {

        edges$width <- 10 * edges$n
        nodes$value <- nodes$centrality
        nodes$id <- nodes$label <- nodes$name
        vn <- visNetwork::visNetwork (nodes, edges,
                                      main = paste0 (pkgmap$name, " network")) %>%
            visNetwork::visEdges (arrows =list (to = list(enabled = TRUE,
                                                          scaleFactor = 0.2)))

        print (vn)
        edges$width <- NULL
        nodes$value <- nodes$label <- nodes$id <- NULL
    }

    # simple numbers of doc/comments lines for each fn (exported +
    # non-exported):
    docs <- doc_lines (pkg_dir, nodes)
    index <- match (docs$name, nodes$name)
    nodes$doc_lines <- docs$doclines [index]
    nodes$cmt_lines <- docs$cmtlines [index]
    nodes$todos <- docs$todos [index]
    nodes$todo_lines <- docs$todo_lines [index]

    # Detailed summaries of fn docs via analyses of /man entries:
    nodes <- get_doc_metrics (pkg_dir, nodes)

    nodes <- num_fun_params (pkg_dir, nodes) # append num_fun_params

    res <- list (nodes = nodes, edges = edges)
    attr (res, "pkg_name") <- pkg_name (pkg_dir)

    return (res)
}

node_centrality <- function (nodes, edges) {

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

    x <- readLines (file.path (pkg_dir, filename), warn = FALSE)
    x <- split (x,
                findInterval (seq (length (x)),
                              nds$line2 + 1)) [seq (nrow (nds))]

    #index <- which (!nodes$export)
    #x <- x [index]

    nlines <- lapply (x, function (i) {
                          i <- i [which (!grepl ("nocov st", i))]
                          ftemp <- file.path (tempdir (), "junk.R")
                          writeLines (i, ftemp)
                          p <- parse (file = ftemp, keep.source = TRUE)
                          p <- utils::getParseData (p)
                          doclines <- which (p$token != "COMMENT") [1] - 1
                          index <- NULL
                          if (!is.na (doclines))
                              index <- which (p$token [(doclines +
                                                        1):nrow (p)] ==
                                              "COMMENT")
                          cmtlines <- length (index)
                          index2 <- grep ("to*do", p$text [index],
                                          ignore.case = TRUE)
                          todos <- length (index2)
                          res <- c (doclines, cmtlines, todos)
                          todo_line_nums <- NA_integer_
                          if (length (index2) > 0)
                              todo_line_nums <- p$line1 [index] [index2]
                          list (res, todo_line_nums)    })

    todo_lines <- lapply (nlines, function (i) i [[2]])
    nlines <- do.call (rbind, lapply (nlines, function (i) i [[1]]))

    res <- data.frame (name = nds$name,
                       doclines = nlines [, 1],
                       cmtlines = nlines [, 2],
                       todos = nlines [, 3],
                       stringsAsFactors = FALSE)

    res$todo_lines <- todo_lines # list column

    return (res)
}

doc_lines <- function (pkg_dir, nodes) {

    files <- unique (nodes$file)
    res <- lapply (files, function (i) doc_lines_one_file (pkg_dir, nodes, i))
    do.call (rbind, res)
}

get_doc_metrics <- function (pkg_dir, nodes) {

    d <- pg_documentation (pkg_dir)
    d <- d [which (names (d) %in% nodes$name)]

    has_usage <- vapply (d, function (i) "usage" %in% names (i), logical (1))
    example_lines <- vapply (d, function (i) {
                                 index <- grep ("^example", names (i))
                                 res <- unname (do.call (c, i [index]))
                                 length (res)
                }, numeric (1))

    nwords <- vapply (d, function (i) {
                          index <- grep ("description|^note", names (i))
                          stringr::str_count (paste (i [index], collapse = " "))
                           }, integer (1))

    index <- match (names (d), nodes$name)
    nodes$n_doc_words <- nodes$n_example_lines <- NA_integer_
    nodes$has_usage <- FALSE

    nodes$n_doc_words [index] <- nwords
    nodes$n_example_lines [index] <- example_lines
    nodes$has_usage [index] <- has_usage

    return (nodes)
}

num_fun_params <- function (pkg_dir, nodes) {

    r_files <- file.path (pkg_dir, unique (nodes$file))

    n <- lapply (r_files, function (i) {

                     x <- parse (text = readLines (i))
                     e <- new.env ()
                     eval (x, envir = e)

                     fns_i <- ls (e)
                     is_fn <- vapply (fns_i, function (f)
                                      class (get (f, envir = e)) == "function",
                                      logical (1))
                     fns_i <- fns_i [which (is_fn)]

                     lens <- vapply (fns_i, function (f)
                                     length (formals (f, envir = e)),
                                     integer (1))
                     has_dots <- vapply (fns_i, function (f)
                                         "..." %in%
                                             names (formals (f, envir = e)),
                                         logical (1))

                     data.frame (name = names (lens),
                                 num_params = as.integer (lens),
                                 has_dots = has_dots,
                                 row.names = NULL,
                                 stringsAsFactors = FALSE)
                           })

    n <- do.call (rbind, n)
    n <- n [which (!duplicated (n)), ]


    nodes <- dplyr::left_join (nodes, n, by = "name")
    nodes$num_params [is.na (nodes$num_params)] <- 0L
    nodes$has_dots [is.na (nodes$has_dots)] <- FALSE

    return (nodes)
}
