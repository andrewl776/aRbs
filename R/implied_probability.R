#' @keywords internal
#'

implied_probability <- function(odds) {
  if (class(odds) == "character")
    odds <- to_decimal(odds)
  1/odds
}
