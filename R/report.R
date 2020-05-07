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
    g$nodes$loc <- g$nodes$line2 - g$nodes$line1 + 1

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

list_collapse <- function (x) {
    if (length (x) > 1)
        x <- paste0 (paste0 (x [-length (x)], collapse = ", "), " and ", x [length (x)])
    return (x)
}


# screen output via cli
cli_out <- function (pkgstats)
{
    message (cli::rule (line = 2, left = pkgstats$pkgname, col = "green"))
    cli::cli_text ("")

    cli::cli_text (cli::col_blue (clusters_out (pkgstats, md = FALSE)))

    cl <- lapply (clusters_list (pkgstats, md = FALSE), function (i)
                  print (knitr::kable (i)))
    cli::cli_text ("")

    if (pkgstats$num_isolated > 0)
    {
        res <- isolated_out (pkgstats, md = FALSE)
        cli::cli_text (res$txt)
        print (knitr::kable (res$iso_fns))
    }
    cli::cli_text ("")

    dl <- doclines_non_exp (pkgstats)
    #cli::cli_text (dl$txt)
    message (dl$txt)
    print (knitr::kable (dl$vals))
}

md_out <- function (g, pkgstats)
{
    out <- c (paste0 ("## ", pkgstats$pkgname), "")

    out <- c (out, clusters_out (pkgstats, md = TRUE), "")

    for (i in clusters_list (pkgstats, md = TRUE))
        out <- c (out, knitr::kable (i, format = "markdown"), "")

    if (pkgstats$num_isolated > 0)
    {
        res <- isolated_out (pkgstats, md = TRUE)
        out <- c (out, res$txt, knitr::kable (res$iso_fns, format = "markdown"))
    }

    out <- c (out, "")

    dl <- doclines_non_exp (pkgstats, md = TRUE)
    out <- c (out, dl$txt, knitr::kable (dl$vals, format = "markdown"))

    return (out)
}

# Summary output of numbers and sizes of clusters
clusters_out <- function (pkgstats, md = FALSE)
{
    cs <- paste0 (pkgstats$cluster_sizes)
    paste0 ("The ", pkgstats$pkg_name, " package has ",
            nrow (pkgstats$exports), " exported functions, and ",
            nrow (pkgstats$non_exports),
            " non-exported funtions. The exported functions are ",
            "structured into the following ",
            pkgstats$num_clusters, " primary cluster",
            ifelse (pkgstats$num_clusters > 1, "s", ""),
            " containing ", list_collapse (cs),
            " function", ifelse (length (cs) > 1, "s", ""))
}

# Summary output of cluster memberships
clusters_list <- function (pkgstats, md = FALSE)
{
    out <- list ()
    for (i in seq (pkgstats$clusters))
    {
        out [[i]] <- data.frame (cluster = i,
                                 n = seq (nrow (pkgstats$clusters [[i]])),
                                 name = pkgstats$clusters [[i]]$name,
                                 num_params = pkgstats$clusters [[i]]$nparams,
                                 num_doc_words = pkgstats$clusters [[i]]$n_doc_words,
                                 num_doc_lines = pkgstats$clusters [[i]]$doc_lines,
                                 num_example_lines = pkgstats$clusters [[i]]$n_example_lines,
                                 centrality = pkgstats$clusters [[i]]$centrality,
                                 row.names = NULL)
    }

    return (out)
}

# Summary output of isolated functions
isolated_out <- function (pkgstats, md = FALSE)
{
    nmtxt <- ifelse (pkgstats$num_isolated > 1, "are", "is")
    out_txt <- paste0 ("There ", nmtxt, " also ", pkgstats$num_isolated,
                       " isolated function",
                       ifelse (pkgstats$num_isolated > 1, "s", ""), ":")
    allfns <- rbind (pkgstats$exports, pkgstats$non_exports)
    iso_fns <- data.frame (n = seq (pkgstats$num_isolated),
                           name = pkgstats$isolated,
                           loc = allfns$loc [match (pkgstats$isolated,
                                                    allfns$name)],
                           row.names = NULL)
    list (txt = out_txt, iso_fns = iso_fns)
}

doclines_non_exp <- function (pkgstats, md = FALSE)
{
    txt <- "Documentation of non-exported functions"
    if (md)
        txt <- c (paste0 ("### ", txt, ":"), "")
    else
        txt <- cli::rule (line = 1, left = txt)

    vals <- data.frame (value = c ("mean", "median"),
                        doclines = c (round (mean (pkgstats$non_exports$doc_lines),
                                             digits = 1),
                                      median (pkgstats$non_exports$doc_lines)),
                        cmtlines = c (round (mean (pkgstats$non_exports$cmt_lines),
                                             digits = 1),
                                      median (pkgstats$non_exports$cmt_lines)))

    list (txt = txt, vals = vals)
}
