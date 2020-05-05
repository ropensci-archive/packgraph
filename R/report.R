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

    pkgstats <- get_pkg_stats (g)

    cli_out (pkgstats)

    invisible (md_out (g, pkgstats))
}

pkg_name <- function (pkg_dir)
{
    desc <- file.path (pkg_dir, "DESCRIPTION")
    x <- readLines (desc)
    strsplit (x [1], "Package: ") [[1]] [2]
}

get_pkg_stats <- function (g)
{
    pkgstats <- list (pkgname = attr (g, "pkg_name"))
    pkgstats$non_exports <- g$nodes [!g$nodes$export, ]
    pkgstats$exports <- g$nodes [g$nodes$export, ]
    export_table <- table (pkgstats$exports$group)

    cluster_groups <- as.integer (names (export_table) [which (export_table > 1)])
    isolated_groups <- as.integer (names (export_table) [which (export_table == 1)])
    clusters <- pkgstats$exports [which (pkgstats$exports$group %in%
                                         cluster_groups), ]
    pkgstats$isolated <- pkgstats$exports [which (pkgstats$exports$group %in%
                                                  isolated_groups), "name",
                                            drop = TRUE]

    # base-r way of gropuing and ordering
    pkgstats$clusters <- lapply (split (clusters, f = factor (clusters$group)),
                                 function (i)
                                 i [order (i$centrality, decreasing = TRUE), ])
    pkgstats$cluster_sizes <- vapply (pkgstats$clusters,
                                      function (i) nrow (i),
                                      integer (1), USE.NAMES = FALSE)

    pkgstats$num_clusters <- length (pkgstats$clusters)
    pkgstats$num_isolated <- length (pkgstats$isolated)

    return (pkgstats)
}


cli_out <- function (pkgstats)
{
    message (cli::rule (line = 2, left = pkgstats$pkgname, col = "green"))
    cli::cli_text ("")

    cs <- paste0 (pkgstats$cluster_sizes)
    txt <- paste0 ("The ", pkgstats$pkgname, " package has ",
                   nrow (pkgstats$exports), " exported functions, and ",
                   nrow (pkgstats$non_exports), "
                   non-exported funtions. The exported functions are ",
                   "structured into the following ",
                   "{pkgstats$num_clusters} primary cluster{?s} containing ",
                   "{.cs {cs}} function{?s}")

    cli::cli_text (cli::col_blue (txt))

    for (i in seq (pkgstats$clusters))
    {
        ci <- data.frame (cluster = i,
                          n = seq (nrow (pkgstats$clusters [[i]])),
                          name = pkgstats$clusters [[i]]$name,
                          centrality = pkgstats$clusters [[i]]$centrality,
                          row.names = NULL)
        print (knitr::kable (ci))
    }
    cli::cli_text ("")

    if (pkgstats$num_isolated > 0)
    {
        nmtxt <- ifelse (pkgstats$num_isolated > 1, "are", "is")
        cli::cli_text ("There ", nmtxt, " also ",
                       "{pkgstats$num_isolated} isolated function{?s}:")
        cli::cli_ol (cli::col_blue (pkgstats$isolated))
    }
}

md_out <- function (g, pkgstats)
{
    out <- c (paste0 ("## ", pkgstats$pkgname), "")

    list_collapse <- function (x) {
        if (length (x) > 1)
            x <- paste0 (paste0 (x [-length (x)], collapse = ", "), " and ", x [length (x)])
        return (x)
    }

    cs <- paste0 (pkgstats$cluster_sizes)
    out <- c (out, paste0 ("The ", pkgstats$pkg_name, " package has ",
                           nrow (pkgstats$exports), " exported functions, and ",
                           nrow (pkgstats$non_exports), "
                           non-exported funtions. The exported functions are ",
                           "structured into the following ",
                           pkgstats$num_clusters, "primary cluster",
                           ifelse (pkgstats$num_clusters > 1, "s", ""),
                           " containing ", list_collapse (cs),
                           " function", ifelse (length (cs) > 1, "s", "")),
              "")

    for (i in seq (pkgstats$clusters))
    {
        ci <- data.frame (cluster = i,
                          n = seq (nrow (pkgstats$clusters [[i]])),
                          name = pkgstats$clusters [[i]]$name,
                          centrality = pkgstats$clusters [[i]]$centrality,
                          row.names = NULL)
        out <- c (out, knitr::kable (ci, format = "markdown"), "")
    }

    if (pkgstats$num_isolated > 0)
    {
        isolated <- pkgstats$isolated
        out <- c (out, paste0 ("There are also ", pkgstats$num_isolated,
                               " isolated functions:"),
                  isolated)
    }

    return (out)
}
