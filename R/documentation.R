#' pg_documentation
#'
#' Extract documentation for one package
#'
#' @param pkg_dir Directory path to package (not a `.tar.gz` archive)
#' @return A list with one item for each package function, each of which
#'         contains a list of all of the variously delinated items defining the
#'         function documentation.
#' @export
pg_documentation <- function (pkg_dir) {
    flist <- list.files (file.path (pkg_dir, "man"), full.names = TRUE)
    flist <- flist [which (grepl ("\\.Rd$", flist))]
    docs <- lapply (flist, function (i) get_one_doc (i))

    names (docs) <- tools::file_path_sans_ext (basename (flist))

    return (docs)
}

#' get_one_doc
#'
#' get documentation for one function
#' @noRd
get_one_doc <- function (rdfile) {
    x <- readLines (rdfile)

    # index of start of all section divisions:
    index <- grep ("^\\\\[[:alpha:]]+\\{", x)
    # but not the following excluded delimiters:
    excludes <- paste0 ("\\\\item\\{|",
                        "\\\\dontrun\\{|",
                        "\\\\url\\{|",
                        "\\\\link\\{|",
                        "\\\\code\\{")
    index <- index [!index %in% grep (excludes, x)]

    # split content between those fields:
    fields <- split (x, findInterval (seq (x), index))

    # remove any non-named fields:
    fields <- fields [vapply (fields,
                              function (i) grepl ("\\\\[[:alpha:]]+\\{", i [1]),
                              logical (1),
                              USE.NAMES = FALSE)]

    # get names of fields
    names (fields) <- vapply (fields,
                              function (i) gsub ("^\\\\|\\{.*", "", i [1]),
                              character (1),
                              USE.NAMES = FALSE)

    # Ensure that these all have officially recognised names
    # https://cran.r-project.org/doc/manuals/R-exts.html#Documenting-functions
    r_ext_names <- c ("name", "alias", "title", "description", "usage",
                      "arguments", "details", "value", "references", "note",
                      "author", "seealso", "examples", "keyword", "section")
    index <- which (!names (fields) %in% r_ext_names)
    index <- index [index > 1] # can do anything about non-compliant initial fields
    while (length (index) > 0) {
        fields [[index [1] - 1]] <- c (fields [[index [1] - 1]],
                                       fields [[index [1] ]])
        fields <- fields [-index [1] ]
        index <- which (!names (fields) %in% r_ext_names)
        index <- index [index > 1]
    }

    # remove those name components
    fields <- lapply (fields, function (i) {
                          i [1] <- gsub ("\\\\[[:alpha:]]+\\{", "", i [1])
                          curly <- FALSE
                          if (grepl ("\\}\\{", i [1])) {
                              curly <- TRUE
                              i [1] <- gsub ("\\}\\{", "", i [1])
                          }
                          if (nchar (i [1]) == 0) i <- i [-1]
                          i [length (i)] <- gsub ("\\}$", "", i [length (i)])
                          while (nchar (utils::tail (i, 1)) == 0)
                              i <- i [-length (i)]
                          if (curly & utils::tail (i, 1) == "}")
                              i <- i [-length (i)]

                          return (i)    })

    # reduce textual fields (description, note) down to concatenated
    # character strings
    concats <- c ("description", "note")
    index <- which (names (fields) %in% concats)
    fields [index] <- lapply (fields [index], function (i)
                              concat_one_entry (i))

    return (fields)
}

concat_one_entry <- function (x) {
    splits <- c (which (x == ""), grep ("^\\s+$", x))
    index <- findInterval (seq (x), splits)
    index [splits] <- NA
    x <- lapply (split (x, as.factor (index)), function (i)
                 paste0 (i, collapse = " "))
    unlist (unname (x))
}
