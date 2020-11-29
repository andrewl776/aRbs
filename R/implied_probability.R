#' @keywords internal
#'

implied_probability <- function(odds) {

  # If all are empty, return NAs
  if (all(odds == "")) return(rep(NA, length(odds)))

  # NA for those that don't have odds
  probs <- odds
  probs[probs == ""] <- NA_real_

  # Those that do have odds _should_ be in fractional format
  probs[!is.na(probs)] <- probs[!is.na(probs)] %>%
    stringr::str_split("/") %>%
    purrr::map(as.numeric) %>%
    purrr::map_dbl(function(s) {s[2]/(s[1] + s[2])})

  # We need as numeric here as we cannot change "" and "0.32838" to be numeric AT THE SAME TIME
  # Each time we try, they all flip back to character as the one we haven't changed is still a
  # character
  probs %>% as.numeric()
}
