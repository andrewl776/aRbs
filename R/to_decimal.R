#' @keywords internal
#'

to_decimal <- function (fractional)
{
  if (inherits(fractional, "data.frame")) {
    return(data.frame(sapply(fractional, to_decimal), row.names = rownames(fractional)) %>%
             stats::setNames(names(fractional)) %>% as.matrix)
  }
  sapply(ifelse(fractional == "", NA, fractional), function(ratio) {
    eval(parse(text = ratio))
  }, USE.NAMES = FALSE) + 1
}
