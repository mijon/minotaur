#' Meeting Minutes
#'
#' Based on `pagedown::html_paged()`.
#'
#' @param css **character** name of css file(s)
#' @md
#' @return An R markdown output format
#' @export
meeting_minutes <- function(css = "default", ...) {
  pagedown::html_paged(
    css = css,
    ...
  )
}



