#' Fix appended form bug
#'
#' This function removes any appended form from a string
#'
#' Sometimes when generating outcome names, the form of a team can be appended
#' to the name. For example, if looking at the odds for St Johnstone FC vs
#' Dundee FC, St Johnstone may be written as St Johnstonewwldd
#' if their previous five games resulted Win, Win, Loss, Draw, Draw. This can
#' be removed with \code{remove_form()}.
#'
#' @param string Any string. If ending in a 5 character combination from the set
#' {"w", "d", "l"} then this will be removed. Otherwise the string will be kept
#' as is.
#'
#' @keywords internal

remove_form <- function(string) {

  ends_in_form <- string %>%
    grepl(pattern = "[w, d, l]{5}$")

  stringr::str_sub(
    string = string,
    1,
    end = stringr::str_length(string) - 5 * ends_in_form
  )
}
