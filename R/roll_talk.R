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

  # https://github.com/quarto-dev/quarto-r/issues/81
  input <- system.file("qmd", "roll_talk.qmd", package = "rolltalk")

  quarto::quarto_render(input, ...)

}
