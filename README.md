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
source("https://install-github.me/r-lib/pkgapi")
```

(or `remotes::install_github()` will also work).

Currently contains a single function, `pg_graph()`, which returns all
exported and non-exported functions from a package (as `nodes`), and
tallies of all functional connections between these (as `edges`). The
function also includes a `plot` parameter which can be used to visualize
the resultant network using the [`visNetwork`
package](https://github.com/datastorm-open/visNetwork).

``` r
git2r::clone ("https://github.com/ropensci/git2r", local_path = "./git2r")
# Currently needs absolute file paths to work
pkg_dir <- tools::file_path_as_absolute ("./git2r")
g <- pg_graph (pkg_dir, plot = FALSE)
g
```

    ## $nodes
    ## # A tibble: 168 x 4
    ##    id                           label                        export group
    ##    <chr>                        <chr>                        <lgl>  <dbl>
    ##  1 .onUnload                    .onUnload                    FALSE      1
    ##  2 [.git_tree                   [.git_tree                   FALSE      1
    ##  3 add                          add                          TRUE       1
    ##  4 add_session_info             add_session_info             FALSE      1
    ##  5 ahead_behind                 ahead_behind                 TRUE       1
    ##  6 as.character.git_time        as.character.git_time        FALSE      1
    ##  7 as.data.frame.git_commit     as.data.frame.git_commit     FALSE      1
    ##  8 as.data.frame.git_repository as.data.frame.git_repository FALSE      1
    ##  9 as.data.frame.git_tree       as.data.frame.git_tree       FALSE      1
    ## 10 as.list.git_tree             as.list.git_tree             FALSE      1
    ## # … with 158 more rows
    ## 
    ## $edges
    ## # A tibble: 202 x 3
    ##    from                         to                    n
    ##    <chr>                        <chr>             <dbl>
    ##  1 [.git_tree                   lookup                1
    ##  2 add                          lookup_repository     1
    ##  3 add                          workdir               1
    ##  4 ahead_behind                 lookup_commit         2
    ##  5 as.data.frame.git_repository commits               1
    ##  6 as.list.git_tree             lookup                1
    ##  7 blame                        lookup_repository     1
    ##  8 blob_create                  lookup_repository     1
    ##  9 branches                     lookup_repository     1
    ## 10 bundle_r_package             clone                 1
    ## # … with 192 more rows

# prior art

The [`collaboratoR` package](https://github.com/bupaverse/collaborateR)
as presented at
[eRum 2020](https://milano-r.github.io/erum2020program/regular-talks.html#using-process-mining-principles-to-extract-a-collaboration-graph-from-a-version-control-system-log).
