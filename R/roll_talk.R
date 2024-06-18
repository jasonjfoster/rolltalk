##' Benchmark Data for Width of 100
##'
##' A data set with the benchmark results for rolling statistics with a `width` of 100.
##'
##' @format A data frame. Rows are evaluation times and columns are the functions.
"bench_100"

##' Benchmark Data for Width of 1,000
##'
##' A data set with the benchmark results for rolling statistics with a `width` of 1,000.
##'
##' @format A data frame. Rows are evaluation times and columns are the functions.
"bench_1000"

##' Rolling and Expanding Statistics
##'
##' A presentation for rolling and expanding statistics of time-series data.
##'
##' @param ... Additional arguments passed to \code{\link[quarto:quarto_render]{quarto::quarto_render}}.
##' @examples
##' \dontrun{
##' # rolling exploratory data analysis
##' roll_talk()
##' }
##' @export
roll_talk <- function(...) {

  quarto_path <- system.file("quarto", package = "rolltalk")

  # https://github.com/quarto-dev/quarto-r/issues/81
  input <- file.path(quarto_path, "roll_talk.qmd")

  quarto::quarto_render(input, ...)

}
