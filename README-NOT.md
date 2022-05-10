# packgraph

<!-- badges: start -->

[![R build
status](https://github.com/mpadge/packgraph/workflows/R-CMD-check/badge.svg)](https://github.com/mpadge/packgraph/actions)
[![Travis build
status](https://travis-ci.org/mpadge/packgraph.svg?branch=master)](https://travis-ci.org/mpadge/packgraph)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Examine and analyse static function call graphs of R packages. Mostly
derived from functionality provided by the [`r-lib/pkgapi`
package](https://github.com/r-lib/pkgapi), which is not on CRAN and must
first be installed with

``` r
remotes::install_github ("r-lib/pkgapi")
```

The main function, `pg_graph()`, returns all exported and non-exported
functions from a package (as `nodes`), and tallies of all functional
connections between these (as `edges`). The function also includes a
`plot` parameter which can be used to visualize the resultant network
using the [`visNetwork`
package](https://github.com/datastorm-open/visNetwork).

``` r
library (packgraph)
if (!file.exists ("./git2r"))
    git2r::clone ("https://github.com/ropensci/git2r", local_path = "./git2r")
# Currently needs absolute file paths to work
pkg_dir <- tools::file_path_as_absolute ("./git2r")
g <- pg_graph (pkg_dir, plot = FALSE)
g
```

    ## $nodes
    ## # A tibble: 168 x 16
    ##    name     export file   line1 line2 group centrality doc_lines cmt_lines todos
    ##    <chr>    <lgl>  <chr>  <int> <int> <int>      <dbl>     <dbl>     <dbl> <dbl>
    ##  1 .onUnlo… FALSE  R/git…    30    32     3        0          21         5     0
    ##  2 [.git_t… FALSE  R/tre…   221   236     1        0          26         0     0
    ##  3 add      TRUE   R/ind…    80    99     1        1.5        50         0     0
    ##  4 add_ses… FALSE  R/com…    70    74     1        0          39         0     0
    ##  5 ahead_b… TRUE   R/com…    57    61     1        0           1         0     0
    ##  6 as.char… FALSE  R/tim…    53    57     4        0           8         0     0
    ##  7 as.data… FALSE  R/com…   627   635     5        0          69         0     0
    ##  8 as.data… FALSE  R/rep…    79    81     1        0           0         0     0
    ##  9 as.data… FALSE  R/tre…    71    77     6        0          25         0     0
    ## 10 as.list… FALSE  R/tre…   114   116     1        0          23         0     0
    ## # … with 158 more rows, and 6 more variables: todo_lines <named list>,
    ## #   n_example_lines <dbl>, n_doc_words <int>, has_usage <lgl>,
    ## #   num_params <int>, has_dots <lgl>
    ## 
    ## $edges
    ## # A tibble: 203 x 3
    ##    from                         to                    n
    ##    <chr>                        <chr>             <int>
    ##  1 [.git_tree                   lookup                1
    ##  2 add                          lookup_repository     1
    ##  3 add                          workdir               1
    ##  4 ahead_behind                 lookup_commit         2
    ##  5 as.data.frame.git_repository commits               1
    ##  6 as.list.git_tree             lookup                1
    ##  7 blame                        lookup_repository     1
    ##  8 blob_create                  lookup_repository     1
    ##  9 branch_create                last_commit           1
    ## 10 branches                     lookup_repository     1
    ## # … with 193 more rows
    ## 
    ## attr(,"pkg_name")
    ## [1] "git2r"

# prior art

-   Much of the functionality is primarily built upon the [`pkgapi`
    package](https://github.com/r-lib/pkgapi).
-   The [`flow` package](https://github.com/moodymudskipper/flow)
    produces flow diagrams of R functions.
-   The [`collaboratoR`
    package](https://github.com/bupaverse/collaborateR) as presented at
    [eRum
    2020](https://milano-r.github.io/erum2020program/regular-talks.html#using-process-mining-principles-to-extract-a-collaboration-graph-from-a-version-control-system-log)
    extracts graphs of *collaborators* from a version control history
    rather than graphs of functions.
