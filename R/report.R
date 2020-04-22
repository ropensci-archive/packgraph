#' pg_report
#'
#' Report on package structure
#'
#' @param g A package graph objected returned from \link{pg_graph}
#' @return Summary report on package strucutre
#' @export
pg_report <- function (g)
{
    if (missing (g))
        stop ("g must be supplied")

    g$nodes$centrality [g$nodes$centrality == 0] <- NA
    non_exports <- g$nodes [!g$nodes$export, ]
    exports <- g$nodes [g$nodes$export, ]
    export_table <- table (exports$group)

    cluster_groups <- as.integer (names (export_table) [which (export_table > 1)])
    isolated_groups <- as.integer (names (export_table) [which (export_table == 1)])
    clusters <- exports [which (exports$group %in% cluster_groups), ]
    isolated <- exports [which (exports$group %in% isolated_groups), "id", drop = TRUE]

    # base-r way of gropuing and ordering
    clusters <- lapply (split (clusters, f = factor (clusters$group)),
                        function (i)
                            i [order (i$centrality, decreasing = TRUE), ])
    cluster_sizes <- vapply (clusters, function (i) nrow (i), integer (1)) %>%
        as.character () # cli_text processing of plurals has to be char

    num_clusters <- length (clusters)
    num_isolated <- length (isolated)

    message (cli::rule (line = 2, left = attr (g, "pkg_name"), col = "green"))
    cli::cli_text ("")
    cli::cli_text (cli::col_blue (paste0 ("   The ", attr (g, "pkg_name"),
                                          " package has ",
                                          nrow (exports),
                                          " exported functions, and ",
                                          nrow (non_exports),
                                          " non-exported funtions\n")))
    cli::cli_text (cli::col_blue (paste0 ("   The exported functions are ",
                                          "structured into the following ",
                                          num_clusters,
                                          " primary clusters ")))
    if (num_isolated == 0)
        cli::cli_text (cli::col_blue (paste0 ("   containing ",
                              "{.cluster_size {cluster_sizes}} function{?s},")))
    else
        cli::cli_text (cli::col_blue (paste0 ("   containing ",
                              "{.cluster_size {cluster_sizes}} function{?s},",
                              " and ", num_isolated, " isolated functions")))

    for (i in seq (clusters))
    {
        ci <- data.frame (cluster = i,
                          n = seq (nrow (clusters [[i]])),
                          name = clusters [[i]]$id,
                          centrality = clusters [[i]]$centrality,
                          row.names = NULL)
        print (knitr::kable (ci))
    }
    cli::cli_text ("")

    cli::cli_text ("The isolated functions are ",
                   cli::col_blue ("{.isolated {isolated}}"))
}

pkg_name <- function (pkg_dir)
{
    desc <- file.path (pkg_dir, "DESCRIPTION")
    x <- readLines (desc)
    strsplit (x [1], "Package: ") [[1]] [2]
}
